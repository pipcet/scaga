#!/usr/bin/perl
use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp read_file/;

my $calls;
eval("\$calls = " . read_file($ARGV[0]));

for my $call (@$calls) {
    my ($caller, $callee, $file, $line, $col, $caller_type, $callee_type, $codeline, $caller_id, $callee_id) = @$call;

    $codeline =~ s/\'//g;

    print "$caller = FLC:$file:$line:$col = \'$codeline\' = $caller_id > $callee = $callee_id\n";
    print "$caller_type > $caller\n";
    print "$callee_type > $callee\n";
}
