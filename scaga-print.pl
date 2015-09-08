#!/usr/bin/perl
use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp read_file/;
use strict;

my $calls;
eval("\$calls = " . read_file($ARGV[0]));

# fix up function pointer function arguments, whose type definition is
# hidden by gimple's syntax beyond the ability of mere regular
# expressions to extract.

sub fixup {
    my ($call) = @_;

    if ($call->{callee_type} =~ /\n/ms) {
        $call->{callee_type} =~ s/.*type = //msg;
    }

    if ($call->{callee_type} =~ /\(\*\*\)/) {
        $call->{callee_type} =~ s/\(\*\*\)/\(\*\)/;
        $call->{callee} = $call->{callee_type};
    }

    if ($call->{callee} =~ /\(\*\)/) {
        $call->{callee_id} = $call->{callee_type};
    }
}

for my $call (@$calls) {
    fixup($call);

    $call->{codeline} =~ s/\'//g;
    $call->{codeline} =~ s/ > />/g;
    $call->{codeline} =~ s/ >> />>/g;
    $call->{codeline} =~ s/ = /=/g;
    $call->{codeline} =~ s/\n/ /msg;
    $call->{caller} =~ s/\* \(\*\)/\*\(\*\)/msg;
    $call->{callee} =~ s/\* \(\*\)/\*\(\*\)/msg;
    $call->{caller} =~ s/\* \*/\*\*/msg;
    $call->{callee} =~ s/\* \*/\*\*/msg;
    $call->{callee} =~ s/(long long|long|short|char) ((un)?signed)( int)?/$2 . " " . $1/mesg;
    $call->{caller} =~ s/(long long|long|short|char) ((un)?signed)( int)?/$2 . " " . $1/mesg;

    $call->{component} = defined($call->{component}) ? (" = component:" . $call->{component}) : "";
    $call->{intype} = (defined($call->{intype}) and $call->{intype} ne "") ? (" = intype:" . $call->{intype}) : "";

    next if ($call->{caller} eq $call->{callee}) and !defined($call->{component}); # XXX distinguish actual recursive
                                # calls from type-only fake calls.

    if (defined($call->{component}) and $call->{caller} eq $call->{callee}) {
        print $call->{caller_type} . $call->{component} . " > " . $call->{caller} . "\n";
    }
    print $caller." = "."FLC:".$file.":".$line.":".$col." = \'".$codeline."\' = ".$caller_id." > ".$callee." = ".$callee_id.$component.$intype ."\n";
    print $call->{caller_type} . " > " . $call->{caller} . "\n" unless $caller_type eq $caller or $caller_type eq "" or $caller eq "";
    print $call->{callee_type} . " > " . $call->{callee} . "\n" unless $callee_type eq $callee or $callee_type eq "" or $callee eq "";
}
