#!/usr/bin/perl
use strict;
use Scaga;
use Getopt::Long;

my $rules = 'rules.scaga';

GetOptions("rules=s" => \$rules);

my @expansions;
my @paths;

while (<>) {
    chomp;

    my $path = Scaga::Path->new($_);

    if ($path->n == 1) {
        my @ppaths = @{$path->{ppaths}};
        if ($ppaths[0]->n == 2) {
            my $rule = Scaga::Rule->new($ppaths[0]->{patterns}->[0]->repr . " => " . $path->repr);
            push @expansions, $rule;
        }
    }

    push @paths, $path;
}

for my $path (@paths) {
    for my $expansion (@expansions) {
        my $m = $path->endmatch($expansion->{in});

        if ($m) {
            my $newpath = $path->slice(0, $m->[0])->concat($expansion->{out});

            print $newpath->repr . "\n";
        }
    }
}
