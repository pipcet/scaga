#!/usr/bin/perl
use IPC::Run qw(run new_chunker);
use File::Slurp qw(read_file write_file);
use Getopt::Long;
use strict;

my $rules_file = 'rules.scaga';
my $exps_file = 'expansions.scaga';
my $maxcycles;

GetOptions("rules=s" => \$rules_file,
           "expansions=s" => \$exps_file,
           "max=i" => \$maxcycles);

my $in = read_file(\*STDIN);
my $out;

my $cycles = 0;

while (!(defined $maxcycles and $cycles >= $maxcycles)) {
    write_file("ser.iteration.$cycles", $in);
    $cycles++;
    my %seen;
    run(["perl", "./scaga-extend.pl", "--rules=$rules_file", "--expansions=$exps_file"], \$in, '>', new_chunker, sub { chomp $_[0]; $seen{$_[0]} = 1; });
    $out = join("\n", sort keys %seen);
    warn "iteration $cycles: " . scalar(keys %seen) . " paths";
    last if $in eq $out;
    $in = $out;
}

print $out;
