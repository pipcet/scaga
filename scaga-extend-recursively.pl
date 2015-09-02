#!/usr/bin/perl
use IPC::Run qw(run new_chunker);
use File::Slurp qw(read_file);
use Getopt::Long;
use strict;

my $rules_file = 'rules.scaga';
my $exps_file = 'expansions.scaga';

GetOptions("rules=s" => \$rules_file,
           "expansions=s" => \$exps_file);

my $in = read_file(\*STDIN);
my $out;

while (1) {
    my %seen;
    run(["perl", "./scaga-extend.pl", "--rules=$rules_file", "--expansions=$exps_file"], \$in, '>', new_chunker, sub { $seen{$_[0]} = 1; });
    $out = join("\n", sort keys %seen);
    warn "in $in out $out";
    last if $in eq $out;
    $in = $out;
}

print $out;
