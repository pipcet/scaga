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
my $do_detect_cycles = 1;
my $do_wait_for_next = 0;
my $last = 4;
my $loop_rules = 1;
my $verbose = 1;

GetOptions("last=i" => \$last,
           "rules=s" => \@rules_files,
           "badrules=s" => \@badrules_files,
           "calls=s" => \@calls_files,
           "loop-rules=i" => \$loop_rules,
           "detect-cycles=i" => \$do_detect_cycles,
           "wait-for-next=i" => \$do_wait_for_next);

sub read_calls {
    my ($file) = @_;

    if (0 && -e "$file.pl") {
        my $ret;
        eval('$ret = ' . read_file("$file.pl"));
        return @$ret;
    } else {
        my @ret;
        my $fh;
        open $fh, "<$file" or die;

        while (<$fh>) {
            chomp;

            my $path = Scaga::Path->new($_);

            if ($path->n == 2) {
                my @ppaths = @{$path->{ppaths}};
                if ($ppaths[0]->n == 2) {
                    my $rule = Scaga::Rule->new($ppaths[0]->{patterns}->[0]->repr . " => " . $path->repr);
                    push @ret, $rule;
                }
            }
        }

        close $fh;

        if (0 && open($fh, ">$file.pl")) {
            print $fh Dumper(\@ret);
            close $fh;
        }

        return @ret;
    }
}

sub hash_calls {
    my (@calls) = @_;
    my $calls = { "" => [] };

    for my $call (@calls) {
        if ($call->{in}->n == 1) {
            my $identifier = $call->{in}->{ppaths}[0]->{patterns}[0]->identifier;

            push @{$calls->{$identifier}}, $call;
        } else {
            push @{$calls->{""}}, $call;
        }
    }

    return $calls;
}

sub hash_rules {
    my (@rules) = @_;
    my $rules = { "" => [] };

    for my $rule (@rules) {
        my @identifiers = $rule->identifiers;

        if (@identifiers) {
            for my $identifier (@identifiers) {
                push @{$rules->{$identifier}}, $rule;
            }
        } else {
            push @{$rules->{""}}, $rule;
        }
    }

    return $rules;
}

warn "reading calls...";
my @calls;
for my $calls_file (@calls_files) {
    push @calls, read_calls($calls_file);
}
my $calls = hash_calls(@calls);

warn "done. " . scalar(@calls) . " calls";

warn "reading paths...";

my @paths;

while (<>) {
    chomp;

    next if $_ eq "";

    my $path = Scaga::Path->new($_);

    push @paths, $path;
}

warn "done. " . scalar(@paths) . " paths";

sub path_expansions {
    my ($path, $rules) = @_;
    my @res;

    my @identifiers = ($path->last_identifier);

    if (!@identifiers) {
        @identifiers = ("");
    }

    my @rules;

    for my $identifier (@identifiers) {
        push @rules, @{$rules->{$identifier}} if $rules->{$identifier};
    }

    for my $rule (@rules) {
        my $s = $rule->substitute($path);

        if ($s) {
            if (@$s) {
                for my $subst (@$s) {
                    my $e = path_expansions($subst, $rules);

                    if ($e) {
                        push @res, @$e;
                    }
                }
            } else {
                return [];
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

 rules_loop:
while ($loop_rules--) {
    warn "reading rules..." if $verbose;
    my %usecount;

    my $oldpaths;
    my $paths;
    for my $path (@paths) {
        $paths->{$path->repr} = $path;
    }
    my $iteration = 0;
    my $notreallydone = 1;
 retry:
    while($notreallydone) {
        my $rules = read_rules(@rules_files);
        my $badrules = read_rules(@badrules_files);
        $notreallydone = 0;

        my $retry = $paths;
        my $do_retry = 0;
        my %oldpaths = $oldpaths ? %$oldpaths : ();

        my %newpaths = %$paths;
        my $newpaths = \%newpaths;
        my @paths = values %$paths;
        $paths = { };

        for my $path (@paths) {
            unless (exists $oldpaths->{$path->short_repr($last, $rules)}) {
                my @calls;
                push @calls, @{$calls->{""}};
                my $identifier = $path->last_identifier;
                push @calls, @{$calls->{$identifier}} if defined $identifier and $calls->{$identifier};
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
                $oldpaths->{$path->short_repr($last, $rules)}->{$path->repr} = 1;
            }
            # delete $paths->{$path->repr};
        }
        @paths = ();

        $rules = read_rules(@rules_files);
        $badrules = read_rules(@badrules_files);
        my $done = 0;
        while (!$done) {
            $done = 1;

            my @newpaths = values %$newpaths;
            $newpaths = { };

            for my $path (@newpaths) {
                next unless defined $path;
                if ($path->cycle) {
                    next;
                }

                my @outpaths;
                my $res = path_expansions($path, $rules);

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

                my $bres = path_expansions($path, $badrules);
                if (@$bres != 1) {
                    warn $path->repr;
                    if ($do_wait_for_next) {
                        my $command = <STDIN>;
                        chomp $command;

                        next if $command eq "--next";
                        next retry if $command eq "--next=retry";
                        next rules_loop if $command eq "--next=rules-loop";
                        die;
                    }
                    next;
                }

                for my $outpath (@outpaths) {
                    unless (exists $oldpaths->{$outpath->short_repr($last, $rules)}) {
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

        for my $hash (values %$rules) {
            for my $rule (@$hash) {
                $usecount{$rule->repr} += $rule->{usecount};
                $rule->{usecount} = 0;
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
            $rules = read_rules(@rules_files);
            $badrules = read_rules(@badrules_files);
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
