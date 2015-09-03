#!/usr/bin/perl
use strict;
use Scaga;
use Getopt::Long;
use File::Slurp qw(read_file write_file);
use Data::Dumper;

my @rules_files = ();
my @exps_files = ();
my $do_detect_cycles = 1;

GetOptions("rules=s" => \@rules_files,
           "expansions=s" => \@exps_files,
           "detect-cycles=i" => \$do_detect_cycles);

sub read_expansions {
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

sub hash_expansions {
    my (@expansions) = @_;
    my $expansions = { "" => [] };

    for my $expansion (@expansions) {
        if ($expansion->{in}->n == 1) {
            my $identifier = $expansion->{in}->{ppaths}[0]->{patterns}[0]->identifier;

            push @{$expansions->{$identifier}}, $expansion;
        } else {
            push @{$expansions->{""}}, $expansion;
        }
    }

    return $expansions;
}

warn "reading expansions...";
my @expansions;
for my $exps_file (@exps_files) {
    push @expansions, read_expansions($exps_file);
}
my $expansions = hash_expansions(@expansions);

warn "done. " . scalar(@expansions) . " expansions";

warn "reading paths...";

my @paths;

while (<>) {
    chomp;

    next if $_ eq "";

    my $path = Scaga::Path->new($_);

    push @paths, $path;
}

warn "done. " . scalar(@paths) . " paths";

warn "expanding paths...";
my @newpaths = @paths;
for my $path (@paths) {
    my @expansions;
    push @expansions, @{$expansions->{""}};
    my $identifier = $path->slice($path->n - 1, $path->n)->{ppaths}[0]->{patterns}[0]->identifier;
    push @expansions, @{$expansions->{$identifier}} if defined $identifier and $expansions->{$identifier};
    for my $expansion (@expansions) {
        my $m = $path->endmatch($expansion->{in});

        if ($m) {
            my $newpath = $path->slice(0, $m->[0])->concat($expansion->{out});

            push @newpaths, $newpath;
            # print $newpath->repr . "\n";
        }
    }
}

warn "done. " . scalar(@newpaths) . " paths";

warn "reading rules...";
my @rules;
for my $rules_file (@rules_files) {
my $fh;
open $fh, "<$rules_file" or die;
while (<$fh>) {
    chomp;

    my $rule = Scaga::Rule->new($_);

    push @rules, $rule;
}
close $fh;
}
warn "done. " . scalar(@rules) . " rules";

sub path_expansions {
    my ($path, $rules) = @_;
    my @res;

    for my $rule (@$rules) {
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

warn "applying rules...";
my $notdone = 1;
while ($notdone) {
    $notdone = 0;

    my @outpaths;

    for my $path (@newpaths) {
        next unless defined $path;
        next if $path->cycle;

        my $res = path_expansions($path, \@rules);

        if ($res) {
            $notdone = 1 if @$res > 1;
            push @outpaths, @$res;
        } else {
            push @outpaths, $path;
        }
    }

    @newpaths = @outpaths;
}
warn "done. " . scalar(@newpaths) . " paths.";

for my $path (@newpaths) {
    print $path->repr . "\n";
}
