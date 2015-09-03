#!/usr/bin/perl
use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp read_file/;

my $calls;
eval("\$calls = " . read_file($ARGV[0]));

for my $call (@$calls) {
    my ($caller, $callee, $file, $line, $col, $caller_type, $callee_type, $codeline, $caller_id, $callee_id, $component) = @$call;

    $codeline =~ s/\'//g;
    $codeline =~ s/ > />/g;
    $codeline =~ s/ >> />>/g;
    $codeline =~ s/ = /=/g;
    $codeline =~ s/\n/ /msg;

    $component = defined($component) ? " = component:$component" : "";

    print "$caller = FLC:$file:$line:$col = \'$codeline\' = $caller_id > $callee = $callee_id$component\n";
    print "$caller_type > $caller\n";
    print "$callee_type > $callee\n";
}
