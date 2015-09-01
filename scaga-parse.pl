#!/usr/bin/perl
use Carp::Always;
use Scaga;

package main;

open $rfh, "<rules.scaga" or die;
my @rules = map { Scaga::Rule->new($_) } <$rfh>;
close $rfh;

 line:
while (<>) {
    chomp;
    my $path = Scaga::Path->new($_);

    for my $rule (@rules) {
        my $m = $path->submatch($rule->{in});

        if ($m and $rule->{out}->n) {
        } else {
        }
    }

    print $path->repr . "\n";
}
