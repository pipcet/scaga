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

sub print_rule {
    my ($kind, @arrays) = @_;
    my @strs;

    for my $i (0 .. $#arrays) {
        @{$arrays[$i]} = grep { defined($_) and $_ ne "" } @{$arrays[$i]};
        return unless @{$arrays[$i]};
        for my $j (0 .. $#{$arrays[$i]}) {
            for my $k ($j+1 .. $#{$arrays[$i]}) {
                if ($arrays[$i][$j] eq $arrays[$i][$k]) {
                    splice @{$arrays[$i]}, $k, 1;
                }
            }
        }
        push @strs, join(" = ", @{$arrays[$i]});
    }

    print "$kind := " . join(" > ", @strs) . "\n";
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
    $call->{codeline} = "'" . $call->{codeline} . "'" if defined $call->{codeline};
    $call->{intype} = "intype:" . $call->{intype} if defined $call->{intype};
    $call->{flc} = "FLC:" . $call->{file} . ":" . $call->{line} . ":" . $call->{col};
    $call->{home} = "home:" . $call->{file} . ":" . $call->{line} . ":" . $call->{col};

    if ($call->{type} eq 'symbol') {
        next if $call->{inexpr} ne $call->{callee_type};

        print_rule("type",
                   [ $call->{caller_type}, $call->{component} ],
                   [ $call->{callee}, $call->{$callee_id} ]);
        # print_rule("type",
        #            [ $call->{caller_type} ],
        #            [ $call->{callee}, $call->{$callee_id} ]);
        next;
    }

    if ($call->{type} ne 'fake' and
        defined($call->{component}) and $call->{caller} eq $call->{callee}) {
        print_rule("call", [$call->{caller_type}],
                   [$call->{callee_type}, $call->{component}]);
    }
    if ($call->{type} ne 'fake') {
        print_rule("call", [$call->{caller}, $call->{flc}, $call->{codeline}],
                   [$call->{callee}, $call->{callee_id}, $call->{component}, $call->{intype}]);
    }
    unless ($call->{caller_type} eq $call->{caller} or $call->{caller_type} eq "" or $call->{caller} eq "") {
        print_rule("type",
                   [$call->{caller_type}],
                   [$call->{caller}]);
    }
    unless ($call->{callee_type} eq $call->{callee} or $call->{callee_type} eq "" or $call->{callee} eq "") {
        print_rule("type",
                   [$call->{callee_type}, $call->{component}],
                   [$call->{callee}]);
    }

    if ($call->{type} eq 'fake') {
        print_rule("home",
                   [$call->{callee}, $call->{home}]);
    }
}
