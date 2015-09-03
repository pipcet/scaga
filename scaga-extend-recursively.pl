#!/usr/bin/perl
use IPC::Run qw(run new_chunker);
use File::Slurp qw(read_file write_file);
use Getopt::Long;
use strict;

my @rules_files = ();
my @calls_files = ();
my $maxcycles;

GetOptions("rules=s" => \@rules_files,
           "calls=s" => \@calls_files,
           "max=i" => \$maxcycles);

my $in = read_file(\*STDIN);
my $out;

my $cycles = 0;

while (!(defined $maxcycles and $cycles >= $maxcycles)) {
    write_file("ser.iteration.$cycles.scaga", $in);
    $cycles++;
    my %seen;
    my @cmd = ("perl", "./scaga-extend.pl");
    for my $rules_file (@rules_files) {
        push @cmd, "--rules=$rules_file";
    }
    for my $calls_file (@calls_files) {
        push @cmd, "--calls=$calls_file";
    }
    run(\@cmd, \$in, '>', new_chunker, sub { chomp $_[0]; $seen{$_[0]} = 1; });
    $out = join("\n", sort keys %seen);
    warn "iteration $cycles: " . scalar(keys %seen) . " paths";
    last if $in eq $out;
    $in = $out;
}

print $out;
