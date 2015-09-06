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
my $last = 4;
my $loop_rules = 1;
my $verbose = 1;

GetOptions("last=i" => \$last,
           "rules=s" => \@rules_files,
           "badrules=s" => \@badrules_files,
           "calls=s" => \@calls_files,
           "loop-rules=i" => \$loop_rules,
           "detect-cycles=i" => \$do_detect_cycles);

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

sub read_rules {
    my @rules_files = @_;
    my @rules;
    for my $rules_file (@rules_files) {
        my $fh;
        open $fh, "<$rules_file" or die;
        while (<$fh>) {
            chomp;
            s/#.*$//;
            next if $_ eq "";

            my $rule = Scaga::Rule->new($_);

            push @rules, $rule;
        }
        close $fh;
    }
    my $rules = hash_rules(@rules);
    warn "done. " . scalar(@rules) . " rules" if $verbose;

    return $rules;
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
        $notreallydone = 0;

        my $retry = $paths;
        my $do_retry = 0;
#        my %oldpaths = $oldpaths ? %$oldpaths : ();

        my %newpaths = %$paths;
        my $newpaths = \%newpaths;
        my @paths = values %$paths;
        $paths = { };

        for my $path (@paths) {
            unless (exists $oldpaths->{$path->short_repr($last)}) {
                my @calls;
                push @calls, @{$calls->{""}};
                my $identifier = $path->last_identifier;
                push @calls, @{$calls->{$identifier}} if defined $identifier and $calls->{$identifier};
                for my $call (@calls) {
                    my $m = $path->endmatch($call->{in});
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
                $oldpaths->{$path->short_repr($last)}->{$path->repr} = 1
                    unless $oldpaths->{$path->short_repr($last)};
            }
            # delete $paths->{$path->repr};
        }
        @paths = ();

        my $rules = read_rules(@rules_files);
        my $badrules = read_rules(@badrules_files);
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
                for my $outpath (@outpaths) {
                    unless (exists $oldpaths->{$outpath->short_repr($last)}) {
                        $paths->{$outpath->repr} = $outpath;
                    }
                }

                my $bres = path_expansions($path, $badrules);
                if (@$bres != 1) {
                    warn "bad path " . $path->repr;
                    $do_retry = 1;
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

#        for my $rule (@rules) {
#            $usecount{$rule->repr} += $rule->{usecount};
#        }

        # my $fh;
        # open $fh, ">rules-last-$last-iteration-$iteration.scaga";
        # for my $rule (sort { $usecount{$b} <=> $usecount{$a} } map { $_->repr } @rules) {
        #     print $fh ($usecount{$rule} . ": " . $rule . "\n");
        # }
        # close $fh;

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
        for my $s (keys %$oldpaths) {
            $oldpaths->{$s} = 1;
        }
        if ($do_retry) {
            $paths = $retry;
            $notreallydone = 1;
#            $oldpaths = \%oldpaths;
            next retry;
        }

        $iteration++;
    }

    for my $path (values %$oldpaths) {
        print $path . "\n";
    }
}
