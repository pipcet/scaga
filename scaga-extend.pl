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
    warn "reading scaga...";
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

    warn "reading scaga done.";
    return @ret;
}

sub hash_scaga {
    warn "hashing scaga...";
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
            } else {
                push @{$ret->{$kind}->{""}}, $rule;
            }
        }
    }

    warn "hashing scaga done.";
    return $ret;
}

my $scaga = hash_scaga(@scaga);
my $scaga1 = hash_scaga(@scaga1);

sub path_expansions {
    my ($path, $scaga, $param) = @_;
    my @res;

    my @identifiers = ($path->last_identifier);

    if (!@identifiers or !$identifiers[0]) {
        @identifiers = ("");
    }

    my @rules;

    $param //= { drop => 1, impossible => 1, subst => 1, "impossible:lto" => 1, "subst:lto" => 1 };

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

# perform an experimental lto-based link to see whether $inpath is a
# code path gcc generates code for if LTO is performed.

# for example:

# int f(int x) { if (x == 0) complicated_function(); return 3; }
# int g(void) { return f(1); }
#
# $inpath = g > f > complicated_function

# result: 0

sub lto_experiment {
    my ($inpath, $scaga, $scaga1) = @_;

    my %type;

    for my $rules (values %{$scaga1->{call}}) {
        for my $rule (@$rules) {
            my $path = $rule->{in};

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
        if (!exists($home{$identifier})) {
            warn "no home for $identifier";
            return 1;
        }
        $files{$home{$identifier}}{$identifier} = 1;
    }

    my @files = sort keys %files;
    my $dir = File::Temp->newdir(CLEANUP => 0);

    for my $file (@files) {
        my $newfile = $file;
        $newfile =~ s/.*\///;
        copy($file, $dir . "/" . $newfile);
        my $fh;
        open $fh, ">>$dir/$newfile" or die "$file / $newfile";

        for my $identifier (sort keys %{$files{$file}}) {
            my $attr = "always_inline";
            $attr = "noinline" if $identifier eq $first_identifier or
                $identifier eq $last_identifier;
            my $proto = $type{$identifier};
            $proto =~ s/\(\*\)/$identifier/;
            print $fh $proto . " __attribute__(($attr));\n";
        }

        system("$cc $dir/$newfile -o $dir/$newfile.o") and return 1;

        close $fh;
    }

    system("$lto -o test.s " . join(" ", map { $dir . "/" . $_ . ".o" } @files)) and return 1;

    my $fh;

    open $fh, "<test.s" or return 1;

    my %called_identifiers;
    while (<$fh>) {
        if (/\.type[ \t]+(.*?),[ \t]*\@function/ and $1 eq $first_identifier) {
            while (<$fh>) {
                last if (/\.size[ \t]+(.*?),/ and $1 eq $first_identifier);

                if (/callq?[ \t]+(.*?)$/) {
                    $called_identifiers{$1} = 1;
                }
            }
            last;
        }
    }

    close $fh;

    for my $called_identifier (sort keys %called_identifiers) {
        warn "function calls $called_identifier\n";
        return 1 if $called_identifier =~ /^\*/; # indirect call
    }

    for my $called_identifier (sort keys %called_identifiers) {
        return 1 if grep { $_ eq $called_identifier } @identifiers;
    }

    return 0;
}

 rules_loop:
while ($loop_rules--) {
    my %usecount;

    my $oldpaths;
    my $paths;
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
        my $keep;
        for my $kind (keys %$scaga) {
            for my $identifier (keys %{$scaga->{$kind}}) {
                $keep->{$identifier} = 1;
            }
        }
        for my $kind (keys %$scaga1) {
            for my $identifier (keys %{$scaga1->{$kind}}) {
                $keep->{$identifier} = 1;
            }
        }
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
            # delete $paths->{$path->repr};
        }
        @paths = ();

        $scaga = hash_scaga(read_scaga(@scaga_files));
        for my $kind (keys %$scaga) {
            for my $identifier (keys %{$scaga->{$kind}}) {
                $keep->{$identifier} = 1;
            }
        }
        for my $kind (keys %$scaga1) {
            for my $identifier (keys %{$scaga1->{$kind}}) {
                $keep->{$identifier} = 1;
            }
        }
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
                    #$done = 0 if @$res > 1;
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
                if (@$bres != 1) {
                    warn $path->repr;
                    while ($do_wait_for_next) {
                        my $command = <STDIN>;
                        chomp $command;

                        next path if $command eq "--next";
                        next retry if $command eq "--next=retry";
                        next rules_loop if $command eq "--next=rules-loop";
                        if ($command =~ /^lto := (.*)$/) {
                            my $path = Scaga::Path->new($1);

                            lto_experiment($path, $scaga, $scaga1);
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

        # warn "shortening paths ..." if $verbose;
        # for my $path (shuffle values %paths) {
        #     my $repr = $path->short_repr($last);
        #     unless (exists $oldpaths{$repr}) {
        #         $oldpaths{$repr} = $path->repr;
        #     }
        # }
        # warn "done. " . scalar(keys %oldpaths) . " paths, iteration " . $iteration . "." if $verbose;

        for my $hash (values %$scaga) {
            for my $rules (values %$hash) {
                for my $rule (@$rules) {
                    $usecount{$rule->repr} += $rule->{usecount};
                    $rule->{usecount} = 0;
                }
            }
        }

        for my $hash (values %$scaga1) {
            for my $rules (values %$hash) {
                for my $rule (@$rules) {
                    $usecount{$rule->repr} += $rule->{usecount};
                    $rule->{usecount} = 0;
                }
            }
        }

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
            for my $kind (keys %$scaga) {
                for my $identifier (keys %{$scaga->{$kind}}) {
                    $keep->{$identifier} = 1;
                }
            }
            for my $kind (keys %$scaga1) {
                for my $identifier (keys %{$scaga1->{$kind}}) {
                    $keep->{$identifier} = 1;
                }
            }
            next retry;
        }

        $iteration++;
    }

    for my $hash (values %$oldpaths) {
        if (ref $hash) {
            for my $path (keys %$hash) {
                print $path . "\n";
            }
        } else {
            print $hash . "\n";
        }
    }
}
