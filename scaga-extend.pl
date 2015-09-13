#!/usr/bin/perl
use strict;
use Scaga;
use Getopt::Long;
use File::Slurp qw(read_file write_file);
use Data::Dumper;
use List::Util qw(shuffle);

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

my @scaga = read_scaga(@scaga_files);
my @scaga1 = read_scaga(@calls_files);

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
    return @ret;
}

sub hash_scaga {
#    warn "hashing scaga...";
    my (@rules) = @_;
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

my $scaga = hash_scaga(@scaga);
my $scaga1 = hash_scaga(@scaga1);

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
    my ($infile, $outfile) = @_;

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

sub lto_experiment {
    my ($inpath, $scaga, $scaga1) = @_;
    my $ltos = lto_read;

    my $string = $inpath->repr;

    if ($ltos->{"lto:impossible := " . $string}) {
        return 'impossible';
    } elsif ($ltos->{"lto:noreturn := " . $string}) {
        return 'noreturn';
    } elsif ($ltos->{"lto:unknown := " . $string}) {
        return 'unknown';
    }

    my @critlines = critical_lines($inpath, $scaga, $scaga1);
    my @critlines1 = critical_lines($inpath, $scaga, $scaga1, 1);

    my %type;

    for my $rules (values %{$scaga1->{call}}) {
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

    my %home;

    for my $rules (values %{$scaga1->{home}}) {
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

    my @identifiers = $inpath->identifiers;
    my $first_identifier = $identifiers[0];
    my $last_identifier = $identifiers[$#identifiers];
    my %files;

    for my $identifier (@identifiers) {
        if ($identifier =~ /\(\*\)/) {
            next;
        }
        if (!exists($home{$identifier})) {
            warn "no home for $identifier";
            return 'unknown';
        }
        $files{$home{$identifier}}{$identifier} = 1;
    }

    my @files = sort keys %files;
    my $dir = File::Temp->newdir(CLEANUP => 0);
    lto_print "# $dir";
    my @newfiles;

    for my $file (@files) {
        my $newfile = $file;
        my $ffile;
        $newfile =~ s/.*\///;
        if ($file !~ /^\//) {
            for my $dir (@source_directories) {
                if (-e "$dir/$file") {
                    $ffile = "$dir/$file";
                    last;
                }
            }
        } else {
            $ffile = $file;
        }
        unless (defined $ffile) {
            warn "can't find $file in @source_directories";
            return 'unknown';
        }
        rewrite($ffile, $dir . "/" . $newfile) or die "$file / $newfile";
        my $fh;
        open $fh, ">>$dir/$newfile" or die "$file / $newfile";

        for my $identifier (sort keys %{$files{$file}}) {
            my $attr = "always_inline";
            $attr = "noinline" if $identifier eq $first_identifier
                or $identifier eq $last_identifier;
            my $proto = $type{$identifier};
            $proto =~ s/\(\*\)/$identifier/;
            print $fh $proto . " __attribute__(($attr));\n";
        }

        warn "$cc $dir/$newfile -o $dir/$newfile.o";
        if (system("$cc $dir/$newfile -o $dir/$newfile.o")) {
            warn "$cc failed";
            return 'unknown';
        }

        close $fh;
        push @newfiles, $newfile;
    }

    warn "$lto -o $dir/test.s " . join(" ", map { $dir . "/" . $_ . ".o" } @newfiles);
    if (system("$lto -o $dir/test.s " . join(" ", map { $dir . "/" . $_ . ".o" } @newfiles))) {
        warn "$lto failed";
        # return 1;
    }

    my $fh;

    unless (open $fh, "<$dir/test.s") {
        warn "no test.s";
        return 'unknown';
    }

    my %called_identifiers;
    my %called_and_returning_identifiers;
    my $function_returns = 0;
    my $found = 0;
    my @files;
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

    if (!$function_returns) {
        warn "function $first_identifier doesn't return!";
    }

    if (!$found) {
        warn "couldn't find $first_identifier";
        return 'unknown';
    }

    close $fh;

    if ($inpath->n == 1) {
        return $function_returns ? 'unknown' : 'noreturn';
    }

    my $noreturn = 0;
    if (!$called_and_returning_identifiers{$last_identifier}) {
        $noreturn = 1;
        for my $i (1 .. $#identifiers - 1) {
            if ($called_and_returning_identifiers{$identifiers[$i]}) {
                $noreturn = 0;
            }
        }

        # lto_print "lto:noreturn := " . $inpath->repr
        #     if $noreturn;
    }

    my @critical_called_identifiers;
    for my $called_identifier (sort keys %called_identifiers) {
        for my $line (@{$called_identifiers{$called_identifier}}) {
            my ($f0, $l0, $c0) = @{$line->[0]};
            warn "function $first_identifier calls $called_identifier after $f0:$l0:$c0\n";
            if ($line->[1]) {
                my ($f1, $l1, $c1) = @{$line->[1]};
                warn "function $first_identifier calls $called_identifier between $f0:$l0:$c0 and $f1:$l1:$c1\n";
            }
            for my $cline (@critlines) {
                if (inrange($line, $cline)) {
                    warn "in range! oh no!";

                    if ($called_and_returning_identifiers{$called_identifier}) {
                        $noreturn = 0;
                    }

                    return ($noreturn ? 'noreturn' : 'unknown') if $called_identifier =~ /^\*/; # indirect call
                }
            }
            for my $cline (@critlines1) {
                if (inrange($line, $cline)) {
                    push @critical_called_identifiers, $called_identifier;
                }
            }
        }
    }

    if ($last_identifier =~ /\(\*\)/) {
        for my $cci (@critical_called_identifiers) {
            my $newpath = $inpath->slice(0, $inpath->n - 1)->concat(Scaga::Path->new($cci));
            lto_print "lto:devirt := " . $inpath->repr . " => " . $newpath->repr;
            print "lto:devirt := " . $inpath->repr . " => " . $newpath->repr . "\n";
            print "!!!sequence: " . $gsequence++ . " more\n";
        }
    }

    for my $called_identifier (sort keys %called_identifiers) {
        return ($noreturn ? 'noreturn' : 'unknown') if grep { $_ eq $called_identifier } @identifiers;
        $called_identifier =~ s/\..*//;
        return ($noreturn ? 'noreturn' : 'unknown') if grep { $_ eq $called_identifier } @identifiers;
    }

    return 'impossible';
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

 rules_loop:
while ($loop_rules--) {
    my %usecount;

    my $oldpaths;
    my $paths = {};
    my $scaga = hash_scaga(read_scaga(@scaga_files));
    my @paths;
    for my $rules (values %{$scaga->{start}}) {
        for my $rule (@$rules) {
            push @paths, $rule->{in};
        }
    }
    for my $path (@paths) {
        $paths->{$path->repr} = $path;
    }
    my $iteration = 0;
    my $notreallydone = 1;
 retry:
    while($notreallydone) {
        my $scaga = hash_scaga(read_scaga(@scaga_files));
        my $keep = generate_keep();
        $notreallydone = 0;

        my $retry = $paths;
        my $do_retry = 0;
        my %oldpaths = $oldpaths ? %$oldpaths : ();

        my %newpaths = %$paths;
        my $newpaths = \%newpaths;
        my @paths = values %$paths;
        $paths = { };

        for my $path (@paths) {
            unless (exists $oldpaths->{$path->short_repr($last, $keep)}) {
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

                        if (0 && $path->slice($m->[0], $m->[0]+1)->match($call->{out}->slice(0, 1))) {
                            # XXX actually merge the matching pattern.
                            $newpath = $path->slice(0, $m->[0]+1)->concat($call->{out}->slice(1, undef));
                        } else {
                            $newpath = $path->slice(0, $m->[0]+$n)->concat_overlapping($call->{out}, $n);
                        }

                        next if $newpath->cycle or !defined($newpath);

                        $newpaths->{$newpath->repr} = $newpath;
                        # print $newpath->repr . "\n";
                    }
                }
                $oldpaths->{$path->short_repr($last, $keep)}->{$path->repr} = 1;
            }
        }
        @paths = ();

        $scaga = hash_scaga(read_scaga(@scaga_files));
        my $done = 0;
        while (!$done) {
            $done = 1;

            my @newpaths = values %$newpaths;
            $newpaths = { };

          path:
            for my $path (@newpaths) {
                next unless defined $path;
                if ($path->cycle) {
                    next;
                }

                my @outpaths;
                my $res = path_expansions($path, $scaga);

                if ($res) {
                    $done = 0 if @$res > 1;
                    for my $outpath (@$res) {
                        next if $outpath->cycle;
                        push @outpaths, $outpath;
                    }
                } else {
                    push @outpaths, $path;
                }
                if (!@outpaths) {
                    next;
                }

                my $param = { baddie => 1 };
                my $bres = path_expansions($path, $scaga, $param);
                if (@$bres != 1 and
                    !baddie($path, $scaga, $scaga1)) {
                    print "baddie := " . $path->repr . "\n";
                    while ($do_wait_for_next) {
                        print "!!!sequence: " . $gsequence++ . " done\n";
                        my $command = <STDIN>;
                        chomp $command;
                        $command =~ s/\A[\n\r]*//msg;

                        next path if $command eq "--next";
                        next retry if $command eq "--next=retry";
                        next rules_loop if $command eq "--next=rules-loop";
                        next retry if $command eq "--reread-rules";
                        if ($command =~ /^lto := (.*)$/) {
                            my $path = Scaga::Path->new($1);

                            my $ret = lto_experiment($path, $scaga, $scaga1);

                            print "lto:$ret := " . $path->repr . "\n";
                            if ($ret) {
                                lto_print "lto:unknown :=  " . $path->repr;
                            } else {
                                lto_print "lto:impossible := " . $path->repr;
                            }
                            next;
                        }
                        die $command;
                    }
                    next;
                }

                for my $outpath (@outpaths) {
                    unless (exists $oldpaths->{$outpath->short_repr($last, $keep)}) {
                        $paths->{$outpath->repr} = $outpath;
                    }
                }
            }
        }
        warn scalar(keys %$paths) . " new paths, " . scalar(keys %$oldpaths) . " old paths.. iteration $iteration, last $last." if $verbose;

        $notreallydone ||= (0 != scalar(keys %$paths));

        my $fh;
        open $fh, ">rules-last-$last-iteration-$iteration.scaga";
        for my $rule (sort { $usecount{$b} <=> $usecount{$a} } keys %usecount) {
            print $fh ($usecount{$rule} . ": " . $rule . "\n");
        }
        close $fh;

        my $fh;
        open $fh, ">se-last-$last-iteration-$iteration.scaga";
        for my $hash (values %$oldpaths) {
            if (ref $hash) {
                for my $path (keys %$hash) {
                    print $fh $path . "\n";
                }
            } else {
                print $fh $hash . "\n";
            }
        }
        close $fh;
        if ($do_retry) {
            $paths = $retry;
            $notreallydone = 1;
            $oldpaths = \%oldpaths;
            $scaga = hash_scaga(read_scaga(@scaga_files));
            next retry;
        }

        $iteration++;
    }
}
