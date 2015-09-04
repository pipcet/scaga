#!/usr/bin/perl
use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp read_file/;

my $calls;
eval("\$calls = " . read_file($ARGV[0]));

# fix up function pointer function arguments, whose type definition is
# hidden by gimple's syntax beyond the ability of mere regular
# expressions to extract.

sub fixup {
    my ($call) = @_;
    my ($caller, $callee, $file, $line, $col, $caller_type, $callee_type, $codeline, $caller_id, $callee_id, $component, $intype) = @$call;

    if ($callee_type =~ /\n/ms) {
        $callee_type =~ s/.*type = //msg;
    }

    if ($callee_type =~ /\(\*\*\)/) {
        $callee_type =~ s/\(\*\*\)/\(\*\)/;
        $callee = $callee_type;
    }

    if ($callee =~ /\(\*\)/) {
        $callee_id = $callee_type;
    }

    @$call = ($caller, $callee, $file, $line, $col, $caller_type, $callee_type, $codeline, $caller_id, $callee_id, $component, $intype);
}

for my $call (@$calls) {
    fixup($call);
    my ($caller, $callee, $file, $line, $col, $caller_type, $callee_type, $codeline, $caller_id, $callee_id, $component, $intype) = @$call;

    $codeline =~ s/\'//g;
    $codeline =~ s/ > />/g;
    $codeline =~ s/ >> />>/g;
    $codeline =~ s/ = /=/g;
    $codeline =~ s/\n/ /msg;
    $caller =~ s/\* \(\*\)/\*\(\*\)/msg;
    $callee =~ s/\* \(\*\)/\*\(\*\)/msg;
    $caller =~ s/\* \*/\*\*/msg;
    $callee =~ s/\* \*/\*\*/msg;
    $callee =~ s/(long long|long|short|char) ((un)?signed)( int)?/$2 . " " . $1/mesg;
    $caller =~ s/(long long|long|short|char) ((un)?signed)( int)?/$2 . " " . $1/mesg;

    $component = defined($component) ? " = component:$component" : "";
    $intype = (defined($intype) and $intype ne "") ? " = intype:$intype" : "";

    next if $caller eq $callee; # XXX distinguish actual recursive
                                # calls from type-only fake calls.

    print "$caller = FLC:$file:$line:$col = \'$codeline\' = $caller_id > $callee = $callee_id$component$intype\n";
    print "$caller_type > $caller\n" unless $caller_type eq $caller or $caller_type eq "" or $caller eq "";
    print "$callee_type > $callee\n" unless $callee_type eq $callee or $callee_type eq "" or $callee eq "";
}
