#!/usr/bin/perl
use strict;
use Scaga;
use Getopt::Long;
use File::Slurp qw(read_file write_file);
use Data::Dumper;
use List::Util qw(shuffle);
use IPC::Run qw(start);
use Generator;

my @rules_files = ();
my @calls_files = ();
my @badrules_files = ();
my @executable_files = ();
my @source_directories = ();
my $do_detect_cycles = 1;
my $do_wait_for_next = 0;
my $last = 1;
my $loop_rules = 1;
my $verbose = 1;
my $cc;
my $lto;
my $gsequence = 0;

GetOptions("last=i" => \$last,
           "rules=s" => \@rules_files,
           "badrules=s" => \@badrules_files,
           "calls=s" => \@calls_files,
           "loop-rules=i" => \$loop_rules,
           "detect-cycles=i" => \$do_detect_cycles,
           "wait-for-next=i" => \$do_wait_for_next,
           "executable=s" => \@executable_files,
           "cc=s" => \$cc,
           "lto=s" => \$lto,
           "source-directory=s" => \@source_directories);

my @scaga_files;
push @scaga_files, @rules_files;
push @scaga_files, @badrules_files;

sub read_scaga {
#    warn "reading scaga...";
    my @ret;
    for my $file (@_) {
        my $fh;
        open $fh, "<$file" or die;

        while (<$fh>) {
            chomp;

            my $rule = Scaga::Rule->new($_);

            if ($rule->{kind} eq "call" or $rule->{kind} eq "type") {
                die if $rule->{out};
                my $path = $rule->{in};
                if ($path->n == 2) {
                    my @ppaths = @{$path->{ppaths}};
                    if ($ppaths[0]->n == 2) {
                        my $rule = Scaga::Rule->new("call := " . $ppaths[0]->{patterns}->[0]->repr . " => " . $path->repr);
                        push @ret, $rule;
                    }
                }
            } else {
                push @ret, $rule;
            }
        }

        close $fh;
    }

#    warn "reading scaga done.";
    return \@ret;
}

sub scaga_rules {
#    warn "hashing scaga...";
    my ($rules) = @_;
    my @rules = @$rules;
    my $ret = { call => { "" => [] }};
    for my $rule (@rules) {
        my $kind = $rule->{kind};

        $ret->{$kind} //= { };

        if ($kind eq "call" or
            $kind eq "type") {
            my $call = $rule;

            if ($call->{in}->n == 1) {
                my $identifier = $call->{in}->{ppaths}[0]->{patterns}[0]->identifier;

                push @{$ret->{call}->{$identifier}}, $call;
            } else {
                push @{$ret->{call}->{""}}, $call;
            }
        } else {
            my @identifiers = $rule->identifiers;

            if (@identifiers) {
                for my $identifier (@identifiers) {
                    push @{$ret->{$kind}->{$identifier}}, $rule;
                }
                for my $i (0 .. $#identifiers - 2) {
                    $ret->{id_id}->{$identifiers[$i]}->{$identifiers[$i+1]} = 1;
                }
            } else {
                push @{$ret->{$kind}->{""}}, $rule;
            }
        }
    }

#    warn "hashing scaga done.";
    return $ret;
}

sub scaga_homes {
    my (@scaga) = @_;
    my %home;
    for my $scaga (@scaga) {
        for my $rules (values %{$scaga->{home}}) {
            for my $rule (@$rules) {
                my $path = $rule->{in};
                next unless $path->n == 1;
                my $identifier = $path->last_identifier;
                my $home;

                for my $component (@{$path->{ppaths}[0]->{patterns}[0]->{components}}) {
                    if (exists $component->{home}) {
                        $home = $component->{home};
                        $home =~ s/:.*//;
                    }
                }

                if (defined $home) {
                    $home{$identifier} = $home;
                }
            }
        }
    }

    return \%home;
}

sub scaga_types {
    my (@scaga) = @_;
    my %type;
    for my $scaga (@scaga) {
        for my $rules (values %{$scaga->{call}}) {
            for my $rule (@$rules) {
                my $path = $rule->{out};

                next unless $path->n == 2;
                my $identifier = $path->last_identifier;

                my $type;

                for my $component (@{$path->{ppaths}[0]->{patterns}[0]->{components}}) {
                    if (exists $component->{identifier}) {
                        $type = $component->{identifier};
                    }
                }

                if (defined $type and $type =~ /\(\*\)/) {
                    $type{$identifier} = $type;
                }
            }
        }
    }

    return \%type;
}

my $scaga = scaga_rules(read_scaga(@scaga_files));
my $scaga1 = scaga_rules(read_scaga(@calls_files));

delete $scaga1->{id_id};

sub path_expansions {
    my ($path, $scaga, $param) = @_;
    my @res;

    my @identifiers = ($path->last_identifier);

    if (!@identifiers or !$identifiers[0]) {
        @identifiers = ("");
    }

    my @rules;

    $param //= { drop => 1, impossible => 1, subst => 1, "lto:impossible" => 1, "lto:devirt" => 1, "lto:noreturn" => 1 };

    for my $identifier (@identifiers) {
        for my $kind (sort keys %$param) {
            push @rules, @{$scaga->{$kind}->{$identifier}} if $scaga->{$kind}->{$identifier};
        }
    }

    for my $rule (@rules) {
        if ($param->{$rule->{kind}}) {
            my $s = $rule->substitute($path);

            if ($s) {
                if (@$s) {
                    for my $subst (@$s) {
                        my $e = path_expansions($subst, $scaga, $param);

                        if ($e) {
                            push @res, @$e;
                        }
                    }
                } else {
                    return [];
                }
            }
        }
    }

    if (@res) {
        return \@res;
    } else {
        return [$path];
    }
}

sub read_rule {
    my ($rule) = @_;
    my @res;

    if ($rule =~ / \| /) {
        my $rule0 = $rule;
        my $rule1 = $rule;

        $rule0 =~ s/( > |^)(.*?) \| (.*?)($| > )/$1$2$4/;
        $rule1 =~ s/( > |^)(.*?) \| (.*?)($| > )/$1$3$4/;

        push @res, read_rule($rule0);
        push @res, read_rule($rule1);
    } else {
        push @res, Scaga::Rule->new($rule);
    }

    return @res;
}

sub read_rules {
    my @rules_files = @_;
    my @rules;
    for my $rules_file (@rules_files) {
        my $line = 0;
        my $fh;
        open $fh, "<$rules_file" or die;
        while (<$fh>) {
            $line++;
            chomp;
            s/#.*$//;
            next if $_ eq "";

            for my $rule (read_rule($_)) {
                $rule->{file} = $rules_file;
                $rule->{line} = $line;

                push @rules, $rule;
            }
        }
        close $fh;
    }
    my $rules = hash_rules(@rules);

    return $rules;
}

{
    my $fh;
    open $fh, ">match.html";
    print $fh <<'EOF';
<html>
  <head>
    <title>SCAGA matches</title>
  </head>
  <body>
EOF

    close $fh;
}

use File::Temp;
use File::Copy;

sub inrange {
    my ($line, $flc) = @_;

    $flc =~ /^(.*?):(.*?):(.*?)$/;
    my ($fc, $lc, $cc) = ($1, $2, $3);

    my ($testa, $testb, $testc);
    if ($fc eq $line->[0]->[0]) {
        $testc = 1;
        if ($lc >= $line->[0]->[1]) {
            $testa = 1;
        } else {
            $testa = 0;
        }
    } else {
        $testa = 1;
    }

    if ($line->[1]) {
        if ($fc eq $line->[1]->[0]) {
            $testc = 1;
            if ($lc < $line->[1]->[1]) {
                $testb = 1;
            } else {
                $testb = 0;
            }
        } else {
            $testb = 1;
        }
    } else {
        $testb = 1;
    }

    return $testa && $testb && $testc;
}

# return the "critical" lines at which the calls making up our call
# chain are made.
sub critical_lines {
    my ($path0, $scaga, $scaga1, $only_last) = @_;
    my @ret;

    for my $rules (values %{$scaga1->{call}}) {
        for my $rule (@$rules) {
            my $path = $rule->{out};

            next unless ($only_last ? $path0->endmatch($path) : $path0->submatch($path));

            next unless $path->n == 2;
            my $identifier = $path->last_identifier;

            my $type;

            for my $component (@{$path->{ppaths}[0]->{patterns}[0]->{components}}) {
                if (exists $component->{flc}) {
                    push @ret, $component->{flc};
                }
            }
        }
    }

    warn "critical lines: " . Dumper(\@ret);

    return @ret;
}

sub generate_keep {
    my $keep = {};
    return $keep;
    for my $kind (keys %$scaga) {
        # for my $identifier (keys %{$scaga->{$kind}}) {
        #     $keep->{$identifier} = 1;
        # }
        for my $rules (values %{$scaga->{$kind}}) {
            for my $rule (@$rules) {
                for my $i (0 .. $rule->{in}->n-1) {
                    $keep->{$rule->{in}->slice($i, $i+1)->repr} = 1;
                }
            }
        }
    }

    return $keep;
}

sub lto_read {
    return {};

    my $fh;
    open $fh, "<lto-auto-rules.scaga" or return;

    my @strings = <$fh>;

    map { chomp $_ } @strings;

    my $ret = {};

    for my $string (@strings) {
        $ret->{$string} = 1;
    }

    return $ret;
}

sub lto_print {
    return;

    my ($string) = @_;

    my $lto = lto_read();

    return if $lto->{$string};

    warn "$string";

    my $fh;
    open $fh, ">>lto-auto-rules.scaga" or return;

    print $fh "$string\n";

    close $fh;
}

# perform an experimental lto-based link to see whether $inpath is a
# code path gcc generates code for if LTO is performed.

# for example:

# int f(int x) { if (x == 0) complicated_function(); return 3; }
# int g(void) { return f(1); }
#
# $inpath = g > f > complicated_function

# result: 0

sub rewrite {
    my ($outfile, $infile) = @_;

    my ($ifh, $ofh);
    open $ifh, "<$infile" or return 0;
    open $ofh, ">$outfile" or return 0;
    my $line = 1;

    while (<$ifh>) {
        chomp;

        if (/_setjmp/) {
            print $ofh "#define _setjmp(x) ({ volatile int xx = 0; (void)(x); *(&xx); })\n";
            print $ofh "#line $line\n";
            print $ofh "$_\n";
        } else {
            print $ofh "$_\n";
        }
        $line++;
    }

    return 1;
}

sub lto_rewrite {
    my ($out, $in, $attrs, $protos) = @_;

    rewrite($out, $in) or die "$in / $out";
    my $fh;
    open $fh, ">>$out" or die "$in / $out";

    for my $identifier (sort keys %{$attrs}) {
        my $attr = $attrs->{$identifier};
        my $proto = $protos->{$identifier};
        $proto =~ s/\(\*\)/$identifier/;
        print $fh $proto . " __attribute__(($attr));\n";
    }

    close $fh;

    return $out;
}

sub lto_cc1 {
    my ($out, $in) = @_;
    my ($stdin, $stdout, $stderr);
    warn "$cc $in -o $out";
    return start(["/bin/sh",  "-c", "$cc $in -o $out"], \$stdin, \$stdout, \$stderr);
}

sub lto_lto1 {
    my ($out, @in) = @_;
    my ($stdin, $stdout, $stderr);
    warn "$lto -o $out " . join(" ", @in);
    return start(["/bin/sh", "-c", "$lto -o $out " . join(" ", @in)], \$stdin, \$stdout, \$stderr);
}

sub lto_find_file {
    my ($file, @directories) = @_;

    if ($file =~ /^\//) {
        return $file;
    }

    for my $directory (@directories) {
        return "$directory/$file" if -e "$directory/$file";
    }

    return;
}

sub lto_read_sfile {
    my ($sfile, $first_identifier) = @_;
    my %called_identifiers;
    my %called_and_returning_identifiers;
    my $function_returns = 0;
    my $found = 0;
    my @files;
    my $fh;

    unless (open $fh, "<$sfile") {
        warn "no test.s";
        return;
    }

    while (<$fh>) {
        if (/\.file[ \t]+(\d*?)[ \t]+\"(.*?)\"$/) {
            $files[$1] = $2;
            $files[$1] =~ s/.*\///;
        }
        if (/\.type[ \t]+(.*?),[ \t]*\@function/ and $1 eq $first_identifier) {
            my ($file, $lineno, $column);
            # a hash of lines that might end up with the function returning.
            my %returning_lines;
            my @open_identifiers;
            my @fcode;
            my %jumps;
            my %labels;
            my $asmlineno = 0;
            my @linenos;
            while (<$fh>) {
                chomp;
                push @fcode, $_;
                last if (/\.size[ \t]+(.*?),/ and $1 eq $first_identifier);

                if (/\.loc[ \t]+(.*?) (.*?) (.*?)$/) {
                    ($file, $lineno, $column) = ($files[$1], $2, $3);

                    push @linenos, [$file, $lineno, $column];

                }
                if (/callq?[ \t]+(.*?)$/) {
                    push @{$called_identifiers{$1}}, [ [$file, $lineno, $column] ];
                    push @open_identifiers, $1;
                }
                if (/^[ \t]*(rep[ \t]+)?ret/) {
                    $function_returns = 1;
                    $returning_lines{$asmlineno} = 1;
                }
                if (/^[ \t]*jmp[ \t]+[a-zA-Z_]/) {
                    warn "potential sibling call $_, you should have compiled with -fno-optimize-sibling-calls!";
                    $function_returns = 1;
                    $returning_lines{$asmlineno} = 1;
                }
                if (/^[ \t]*j[a-z]*[ \t](.*?)$/) {
                    $jumps{$1}{$asmlineno} = 1;
                }
                unless (/^[ \t]*(jmpq?)/) {
                    $jumps{$asmlineno+1}{$asmlineno} = 1;
                }
                if (/^([.a-zA-Z0-9_]*):/) {
                    $labels{$asmlineno}{$1} = $1;
                }
                $asmlineno++;
            }
            my @linenos = sort { $a->[0] cmp $b->[0] || $a->[1] <=> $b->[1] } @linenos;

          id:
            for my $identifier (@open_identifiers) {
                my $lines = $called_identifiers{$identifier};
                my $line = $lines->[$#$lines];
                my $i = 0;

                for my $i (reverse (0 .. $#linenos-1)) {
                    if ($linenos[$i]->[0] eq $line->[0]->[0] and
                        $linenos[$i]->[1] == $line->[0]->[1]) {
                        warn Dumper($linenos[$i]) . Dumper($linenos[$i+1]);
                        $line->[1] = $linenos[$i+1];
                        next id;
                    }
                }
            }

            @open_identifiers = ();
            $found = 1;

            my $done = 0;
            while (!$done) {
                $done = 1;
                for my $asmlineno (keys %returning_lines) {
                    for my $label (keys %{$labels{$asmlineno}}) {
                        $done = 0 unless $returning_lines{$label};
                        $returning_lines{$label} = 1;
                    }
                    for my $origin (keys %{$jumps{$asmlineno}}) {
                        $done = 0 unless $returning_lines{$origin};
                        $returning_lines{$origin} = 1;
                    }
                }
            }
            for my $asmlineno (keys %returning_lines) {
                if ($asmlineno =~ /^[0-9]*$/) {
                    if ($fcode[$asmlineno] =~ /callq?[ \t]+(.*?)$/) {
                        my $identifier = $1;
                        $called_and_returning_identifiers{$identifier} = 1;
                        $identifier =~ s/\..*//;
                        $called_and_returning_identifiers{$identifier} = 1;
                    }
                }
            }
        }
    }

    close $fh;

    return (!$function_returns, \%called_and_returning_identifiers, \%called_identifiers);
}

sub lto_critical_identifiers {
    my ($called_identifiers, $called_and_returning_identifiers, $critlines, $critlines1) = @_;
    my @critical_called_identifiers;

    for my $called_identifier (sort keys %$called_identifiers) {
        for my $line (@{$called_identifiers->{$called_identifier}}) {
            for my $cline (@$critlines) {
                if (inrange($line, $cline)) {
                    warn "in range! oh no!";

                    if ($called_identifier =~ /^\*/) {
                        # indirect call
                        return '(*)';
                    }
                }
            }

            for my $cline (@$critlines1) {
                if (inrange($line, $cline)) {
                    push @critical_called_identifiers, $called_identifier;
                }
            }
        }
    }

    return @critical_called_identifiers;
}

sub lto_cfiles {
    my ($home, @identifiers) = @_;
    my $cfiles = {};

    for my $identifier (@identifiers) {
        if ($identifier =~ /\(\*\)/) {
            next;
        }
        if (!exists($home->{$identifier})) {
            return;
        }
        $cfiles->{$home->{$identifier}}{$identifier} = 1;
    }

    return $cfiles;
}

sub lto_experiment {
    my ($inpath, $scaga, $scaga1) = @_;
    my @res;

    my $string = $inpath->repr;

    my $type = scaga_types($scaga1);
    my $home = scaga_homes($scaga1);

    my @critlines = critical_lines($inpath, $scaga, $scaga1);
    my @critlines1 = critical_lines($inpath, $scaga, $scaga1, 1);

    my @identifiers = $inpath->identifiers;
    my $first_identifier = $identifiers[0];
    my $last_identifier = $identifiers[$#identifiers];

    my $cfiles = lto_cfiles($home, @identifiers);

    my @cfiles = sort keys %$cfiles;

    my $dir = File::Temp->newdir(CLEANUP => 0);
    my %oh;
    my @ofiles;
    my $sh;
    my $sfile;
    my $yield;
    my $state = 0;

    for my $cfile (@cfiles) {
        my $ffile = lto_find_file($cfile, @source_directories);

        if (!defined $ffile) {
            warn "can't find $cfile in @source_directories";
            return \@res;
        }

        for my $identifier (sort keys %{$cfiles->{$cfile}}) {
            my $attr = "always_inline";
            $attr = "noinline" if $identifier eq $first_identifier
                or $identifier eq $last_identifier;
            $cfiles->{$cfile}{$identifier} = $attr;
        }

        my $cfile_rewritten = lto_rewrite("$dir/$cfile", $ffile, $cfiles->{$cfile}, $type);

        if (!defined($cfile_rewritten)) {
            return \@res;
        }

        $oh{"$dir/$cfile.o"} = lto_cc1("$dir/$cfile.o", $cfile_rewritten);
        $oh{"$dir/$cfile.o"}->start;
    }

    $yield = sub {
        if ($state == 0) {
            for my $ofile (keys %oh) {
                $oh{$ofile}->finish or return @res;
                push @ofiles, $ofile;
            }
            $sh = lto_lto1("$dir/test.s", @ofiles);

            return;
        } elsif ($state == 1) {
            $sh->finish or return @res;
            $sfile = "$dir/test.s";

            my ($noreturn, $called_and_returning_identifiers, $called_identifiers) =
                lto_read_sfile($sfile);

            if (!defined $noreturn) {
                return @res;
            }

            if ($called_and_returning_identifiers->{$last_identifier}) {
                $noreturn = 1;
                for my $i (1 .. $#identifiers - 1) {
                    if ($called_and_returning_identifiers->{$identifiers[$i]}) {
                        $noreturn = 0;
                    }
                }
            }

            if ($noreturn) {
                push @res, "lto:noreturn := $string\n";
            }

            if ($inpath->n == 1) {
                return @res;
            }

            my @critical_called_identifiers = lto_critical_identifiers($called_identifiers, \@critlines, \@critlines1);

            if ($critical_called_identifiers[0] eq '(*)') {
                return @res;
            }

            if ($last_identifier =~ /\(\*\)/) {
                for my $cci (@critical_called_identifiers) {
                    my $newpath = $inpath->slice(0, $inpath->n - 1)->concat(Scaga::Path->new($cci));
                    push @res, "lto:devirt := " . $inpath->repr . " => " . $newpath->repr;
                }
            }

            for my $called_identifier (sort keys %$called_identifiers) {
                if ($called_identifier eq $last_identifier) {
                    return @res;
                }
                $called_identifier =~ s/\..*//;
                if ($called_identifier eq $last_identifier) {
                    return @res;
                }
            }

            push @res, "lto:impossible := $string";
            return @res;
        }
    };

    return Generator->new(yield => $yield);
}

sub baddie {
    my ($path, $scaga, $scaga1) = @_;

    # warn "baddie " . $path->repr;

    my $n = $path->n;

    for my $i (0 .. $n-1) {
        for my $l (1 .. 5) {
            my $j = $i + $l;
            next if $j > $n;
            my $subpath = $path->slice($i, $j);
            my $ret = 'unknown'; # lto_experiment($subpath, $scaga, $scaga1);

            if ($ret eq 'unknown') {
                lto_print "lto:unknown := " . $subpath->repr;
            } elsif ($ret eq 'noreturn') {
                return 1;
            } elsif ($ret eq 'impossible') {
                lto_print "lto:impossible := " . $subpath->repr;

                return 1;
            } else {
                die "$ret";
            }
        }
    }

    return 0;
}

sub extend_path {
    my ($path_string, $scaga, $scaga1) = @_;
    my @res;
    my $path = Scaga::Path->new($_[0]);
    my @calls;
    push @calls, @{$scaga1->{call}{""}};
    my $identifier = $path->last_identifier;
    push @calls, @{$scaga1->{call}{$identifier}} if defined $identifier and $scaga1->{call}{$identifier};
    for my $call (@calls) {
        my $match_param = { lstrict => { component => 1 }};
        my $m = $path->endmatch($call->{in}, $match_param);
        my $n = $call->{in}->n;

        if ($m) {
            my $newpath;

            $newpath = $path->slice(0, $m->[0]+$n)->concat_overlapping($call->{out}, $n);

            next if $newpath->cycle or !defined($newpath);

            push @res, "extend := " . $newpath->repr;
        }
    }

    return @res;
}

sub expand_path {
    my ($path_string, $scaga, $scaga1) = @_;
    my @res;
    my $path = Scaga::Path->new($_[0]);
    next unless defined $path;
    if ($path->cycle) {
        next;
    }

    my @outpaths;
    my $res = path_expansions($path, $scaga);

    if ($res) {
        for my $outpath (@$res) {
            next if $outpath->cycle;
            push @res, "expand := " . $outpath->repr;
        }
    } else {
        push @res, "expand := " . $path->repr;
    }

    return @res;
}

sub check_path {
    my ($path_string, $scaga, $scaga1) = @_;
    my @res;
    my $path = Scaga::Path->new($_[0]);

    my $param = { baddie => 1 };
    my $bres = path_expansions($path, $scaga, $param);
    if (@$bres != 1 and
        !baddie($path, $scaga, $scaga1)) {
        push @res, "baddie := " . $path->repr;
    }

    return @res;
}

sub lto_path {
    my ($path_string, $scaga, $scaga1) = @_;
    my $path = Scaga::Path->new($_[0]);

    return lto_experiment($path, $scaga, $scaga1);
}

sub rec1_path {
    my ($path_string, $scaga, $scaga1) = @_;
    my @res;

    my @extended = extend_path($path_string, $scaga, $scaga1);
    my @expanded;

    for my $extended (@extended) {
        $extended =~ s/^extend := //;

        push @expanded, expand_path($extended, $scaga, $scaga1);
    }
    @extended = ();

    for my $expanded (@expanded) {
        $expanded =~ s/^expand := //;
        push @res, check_path($expanded, $scaga, $scaga1);
        print "expand := $expanded\n";
        rec_path($expanded, $scaga, $scaga1);
    }

    return @res;
}

sub rec0_path {
    my ($path_string, $scaga, $scaga1) = @_;
    my @res;
    my %seen;
    my @new = ($path_string);
    my $done = 0;

    while (!$done) {
        my @newnew;
        $done = 1;

        warn scalar(keys %seen) . " paths";

        for my $path_string (@new) {
            next if $seen{$path_string};
            $done = 0;

            my @extended = extend_path($path_string, $scaga, $scaga1);
            my @expanded;

            for my $extended (@extended) {
                $extended =~ s/^extend := //;

                push @expanded, expand_path($extended, $scaga, $scaga1);
            }

            for my $expanded (@expanded) {
                $expanded =~ s/^expand := //;
                push @res, check_path($expanded, $scaga, $scaga1);
                push @res, "expand := $expanded";
                push @newnew, $expanded;
            }
            $seen{$path_string} = 1;
        }

        @new = @newnew;
    }

    return @res;
}

my $lasttime = 0;
sub rec_path {
    my ($path_string, $scaga, $scaga1) = @_;
    my @res;
    my %seen;
    my @new = ($path_string);
    my $done = 0;
    my @newnew;
    my @out;
    my @expanded;
    my @extended;

    my $yield;
    my $abort;

    $abort = sub {
        undef $yield;
        undef $abort;
    };

    $yield = sub {
        my ($self) = @_;
        while (1) {
            if (@out) {
                my @r = @out;
                @out = ();
                return @r;
            }

            if (@expanded) {
                my $expanded = pop @expanded;

                $expanded =~ s/^expand := //;
                if ($expanded =~ /Ffuncall/) {
                    push @out, "baddie := $expanded";
                } else {
                    push @newnew, $expanded;
                }

                next;
            }

            if (@extended) {
                my $extended = pop @extended;

                $extended =~ s/^extend := //;

                push @expanded, expand_path($extended, $scaga, $scaga1);

                next;
            }

            if (@new) {
                my $path_string = pop @new;
                my @identifiers = Scaga::Path->new($path_string)->identifiers;
                my $l;
              L:
                for ($l = 2; $l <= $#identifiers; $l++) {
                    for my $o (reverse 0 .. ($#identifiers + 1 - $l)) {
                        for my $i ($o .. $#identifiers - 1) {
                            next if $scaga->{id_id}->{$identifiers[$i]}->{$identifiers[$i+1]};

                            last L;
                        }
                    }
                }

                if ($l < 2) {
                    $l = $#identifiers + 1;
                }
                splice @identifiers, 0, $#identifiers + 1 - $l;
                my $short_string = join(" > ", @identifiers);

                next if $seen{$short_string};
                @extended = extend_path($path_string, $scaga, $scaga1);
                $seen{$short_string} = 1;
                $done = 0;
                next;
            }

            if (!@new) {
                if ($done) {
                    undef $yield;
                    $self->eof(1);
                    return;
                }

                $done = 1;
                @new = @newnew;
                @newnew = ();

                next;
            }
        }
    };

    return Generator->new(yield => $yield, abort => $abort);
}

my $scaga = scaga_rules(read_scaga(@scaga_files));
my $keep = generate_keep();
my @res = ("init := init\n");

my %generators;

sub sequence_marker {
    my ($more) = @_;

    print "!!!sequence: " . $gsequence++ . ($more ? " more " : " done ") . join(" ", sort { $generators{$a}->{age} <=> $generators{$b}->{age} } (keys %generators)) . "\n";
}

while (1) {
    if (@res) {
        for my $i (0 .. $#res) {
            print $res[$i] . "\n";
            sequence_marker(1)
                unless $#res == $i;
        }
    } else {
        print "nop := nop\n";
    }
    sequence_marker(0);
    @res = ();
    my $command = <STDIN>;
    chomp $command;
    $command =~ s/\A[\n\r]*//msg;

    if ($command =~ /^extend := (.*)$/) {
        @res = extend_path($1, $scaga, $scaga1);
    } elsif ($command =~ /^expand := (.*)$/) {
        @res = expand_path($1, $scaga, $scaga1);
    } elsif ($command =~ /^check := (.*)?/) {
        @res = check_path($1, $scaga, $scaga1);
    } elsif ($command eq "--reread-rules") {
        $scaga = scaga_rules(read_scaga(@scaga_files));
        $keep = generate_keep();
    } elsif ($command =~ /^lto := (.*)$/) {
        $generators{$gsequence} = lto_path($1, $scaga, $scaga1);
    } elsif ($command =~ /^rec := (.*)$/) {
        $generators{$gsequence} = rec_path($1, $scaga, $scaga1);
    } elsif ($command =~ /^--abort (\d*)$/) {
        my $i = $1;
        delete($generators{$i})->abort;
    } elsif ($command =~ /^--next (\d*)$/) {
        my $i = $1;
        push @res, $generators{$i}->fetch;

        delete $generators{$i} if $generators{$i}->eof;
    } elsif ($command eq "") {
        last;
    } else {
        die $command;
    }
}

$SIG{INT} = sub {
    warn "already at top level.";
};
