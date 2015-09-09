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

    $call->{codeline} =~ s/\n/ /msg;
    $call->{codeline} =~ s/\'//g;
    $call->{codeline} =~ s/ *> */>/g;
    $call->{codeline} =~ s/ *>> */>>/g;
    $call->{codeline} =~ s/ *= */=/g;
    $call->{codeline} =~ s/ *:= */=/g;
    $call->{codeline} =~ s/ *\| */\|/g;

    $call->{caller} =~ s/\* \(\*\)/\*\(\*\)/msg;
    $call->{caller} =~ s/\* \*/\*\*/msg;
    $call->{caller} =~ s/(long long|long|short|char) ((un)?signed)( int)?/$2 . " " . $1/mesg;

    $call->{callee} =~ s/\* \(\*\)/\*\(\*\)/msg;
    $call->{callee} =~ s/\* \*/\*\*/msg;
    $call->{callee} =~ s/(long long|long|short|char) ((un)?signed)( int)?/$2 . " " . $1/mesg;

    $call->{component} =~ s/.*(\.|->)//msg;
    $call->{component} = "component:" . $call->{component} if defined $call->{component};
    $call->{intype} = "intype:" . $call->{intype} if defined $call->{intype};
    $call->{flc} = "FLC:" . $call->{file} . ":" . $call->{line} . ":" . $call->{col};
    $call->{home} = "home:" . $call->{file} . ":" . $call->{line} . ":" . $call->{col};
    my @comp0;
    push @comp0, $call->{caller_type};
    push @comp0, $call->{component} if defined $call->{component};

    my @comp1;
    push @comp1, $call->{callee_type};

    my @comp2;
    push @comp2, $call->{caller};
    push @comp2, $call->{flc} if defined $call->{flc};
    push @comp2, "'" . $call->{codeline} . "'" if defined $call->{codeline};
    push @comp2, $call->{caller_id};

    my @comp3;
    push @comp3, $call->{callee};
    push @comp3, $call->{callee_id};
    push @comp3, $call->{component} if defined $call->{component};
    push @comp3, $call->{intype} if defined $call->{intype};

    if ($call->{type} eq 'symbol') {
        next if $call->{inexpr} ne $call->{callee_type};

        my @compa;
        push @compa, $call->{caller_type};
        push @compa, $call->{component};

        my @compb;
        push @compb, $call->{callee};
        push @compb, $call->{callee_id};

        print "type := " . join(" = ", @compa) . " > " . join(" = ", @compb) . "\n";
        pop @compa;
        print "type := " . join(" = ", @compa) . " > " . join(" = ", @compb) . "\n";
        next;
    }

    #next if ($call->{caller} eq $call->{callee}) and !defined($call->{component}); # XXX distinguish actual recursive
                                # calls from type-only fake calls.

    if ($call->{type} ne 'fake' and
        defined($call->{component}) and $call->{caller} eq $call->{callee}) {
        print "call := " . join(" = ", @comp0) . " > " . join(" = ", @comp1) . "\n";
    }
    if ($call->{type} ne 'fake') {
        print "call := " . join(" = ", @comp2) . " > " . join(" = ", @comp3) . "\n";
    }
    print "type := " . $call->{caller_type} . " > " . $call->{caller} . "\n" unless $call->{caller_type} eq $call->{caller} or $call->{caller_type} eq "" or $call->{caller} eq "";
    print "type := " . $call->{callee_type} . " > " . $call->{callee} . "\n" unless $call->{callee_type} eq $call->{callee} or $call->{callee_type} eq "" or $call->{callee} eq "";
    print "home := " . $call->{callee} . " = " . $call->{home} . "\n" if $call->{type} eq 'fake';
}
