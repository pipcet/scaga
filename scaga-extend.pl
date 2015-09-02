#!/usr/bin/perl
use strict;
use Scaga;
use Getopt::Long;

my $rules_file = 'rules.scaga';
my $exps_file = 'expansions.scaga';

GetOptions("rules=s" => \$rules_file,
           "expansions=s" => \$exps_file);

my @expansions;

my $fh;
open $fh, "<$exps_file" or die;

while (<$fh>) {
    chomp;

    my $path = Scaga::Path->new($_);

    if ($path->n == 1) {
        my @ppaths = @{$path->{ppaths}};
        if ($ppaths[0]->n == 2) {
            my $rule = Scaga::Rule->new($ppaths[0]->{patterns}->[0]->repr . " => " . $path->repr);
            push @expansions, $rule;
        }
    }
}

close $fh;

my @paths;

while (<>) {
    chomp;

    my $path = Scaga::Path->new($_);

    push @paths, $path;
}

my @newpaths;
for my $path (@paths) {
    for my $expansion (@expansions) {
        my $m = $path->endmatch($expansion->{in});

        if ($m) {
            my $newpath = $path->slice(0, $m->[0])->concat($expansion->{out});

            push @newpaths, $newpath;
            print $newpath->repr . "\n";
        }
    }
}

my @rules;
open $fh, "<$rules_file" or die;
while (<$fh>) {
    chomp;

    my $rule = Scaga::Rule->new($_);

    push @rules, $rule;
}
close $fh;

sub path_expansions {
    my ($path, $rules) = @_;
    my @res;

    for my $rule (@$rules) {
        my $s = $rule->substitute($path);

        if ($s) {
            if (@$s) {
                for my $subst (@$s) {
                    push @res, path_expansions($subst, $rules);
                }
            } else {
                return [];
            }
        }
    }

    if (@res) {
        return \@res;
    } else {
        return undef;
    }
}

my $notdone = 1;
while ($notdone) {
    $notdone = 0;

    my @outpaths;

    for my $path (@newpaths) {
        my $res = path_expansions($path, \@rules);

        if ($res) {
            $notdone = 1;
            push @outpaths, @$res;
        } else {
            push @outpaths, $path;
        }
    }

    @newpaths = @outpaths;
}

for my $path (@newpaths) {
    print $path->repr . "\n";
}
