#!/usr/bin/perl

use Carp::Always;
package Scaga::Component;
sub repr {
    my ($self) = @_;

    die;
}


sub match {
    my ($self, $other) = @_;

    die if (defined($self->{pattern}));

    if (defined($self->{codeline}) && defined($other->{pattern})) {
        my $pattern = $other->{pattern};

        return $self->{codeline} =~ /$pattern/;
    }

    for my $lkey (keys %{$self}) {
        if (defined($other->{$lkey}) and
            $other->{$lkey} ne $self->{$lkey}) {
            return 0;
        }
    }

    return 1;
}

sub new {
    my ($class, $string, $output) = @_;

    if ($string =~ /^FLC:(.*)$/) {
        return Scaga::Component::FLC->new($1);
    } elsif ($string =~ /^(\/.*\/)$/) {
        return Scaga::Component::RegExp->new($1);
    } elsif ($string =~ /^\'(.*)\'$/) {
        return Scaga::Component::Codeline->new($1);
    } elsif ($string =~ /0x/) {
        return Scaga::Component::Value->new($string);
    } else {
        return Scaga::Component::Identifier->new($string);
    }
}

package Scaga::Component::RegExp;
use parent -norequire, 'Scaga::Component';

sub repr {
    my ($self) = @_;

    return '/' . $self->{pattern} . '/';
}

sub new {
    my ($class, $string, $output) = @_;

    die unless $string =~ /\A\/(.*)\/\Z/;

    return bless { pattern => $1 }, $class;
}

package Scaga::Component::Identifier;
use parent -norequire, 'Scaga::Component';

sub repr {
    my ($self) = @_;

    return $self->{identifier};
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { identifier => $string }, $class;
}

package Scaga::Component::Codeline;
use parent -norequire, 'Scaga::Component';

sub repr {
    my ($self) = @_;

    return "'" . $self->{codeline} . "'";
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { codeline => $string }, $class;
}

package Scaga::Component::FLC;
use parent -norequire, 'Scaga::Component';

sub repr {
    my ($self) = @_;

    return "FLC:" . $self->{flc};
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { flc => $string }, $class;
}

package Scaga::Component::Value;
use parent -norequire, 'Scaga::Component';

sub repr {
    my ($self) = @_;

    return $self->{value};
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { value => $string }, $class;
}

package Scaga::Pattern;

sub repr {
    my ($self) = @_;

    return join(" = ", map { $_->repr } @{$self->{components}});
}

sub match {
    my ($self, $other) = @_;

    for my $lcomp (@{$self->{components}}) {
        for my $rcomp (@{$other->{components}}) {
            if (!$lcomp->match($rcomp)) {
                return 0;
            }
        }
    }

    return 1;
}

sub new {
    my ($class, $string, $output) = @_;

    my @components = split(" = ", $string);

    @components = map { Scaga::Component->new($_) } @components;

    return bless { components => \@components }, $class;
}

package Scaga::PPath;

sub repr {
    my ($self) = @_;

    return join(" > ", map { $_->repr } @{$self->{patterns}});
}

sub slice {
    my ($self, $i, $k) = @_;

    my @spatterns = @{$self->{patterns}}[$i .. $i+$k-1];

    return bless { patterns => \@spatterns }, 'Scaga::PPath';
}

sub match {
    my ($self, $other) = @_;

    die unless $other->isa('Scaga::PPath');

    my $ln = scalar(@{$self->{patterns}});
    my $rn = scalar(@{$other->{patterns}});

    if ($ln == $rn) {
        my $n = $ln;

        for my $i (0..$n-1) {
            if (!$self->{patterns}->[$i]->match($other->{patterns}->[$i])) {
                return 0;
            }
        }

        return 1;
    }

    return 0;
}

sub new {
    my ($class, $string, $output) = @_;

    my @patterns = split(" > ", $string);

    @patterns = map { Scaga::Pattern->new($_) } @patterns;

    return bless { patterns => \@patterns }, $class;
}

package Scaga::Path;

sub repr {
    my ($self) = @_;

    return join(" >> ", map { $_->repr } @{$self->{ppaths}});
}

sub n {
    my ($self) = @_;

    return scalar(@{$self->{ppaths}});
}

sub new {
    my ($class, $string, $output) = @_;

    my @ppaths = split(" >> ", $string);

    @ppaths = map { Scaga::PPath->new($_) } @ppaths;

    return bless { ppaths => \@ppaths }, $class;
}

sub increasing_sequences {
    my ($k, $n, $o) = @_;

    $o //= 0;

    if ($k == 0) {
        return ([]);
    }

    my @ret;
    for my $n0 ($o .. $n-$k) {
        for my $s (increasing_sequences($k-1, $n, $o + $n0 + 1)) {
            push @ret, [$n0, @$s];
        }
    }
    return @ret;
}

sub submatch {
    my ($self, $other) = @_;

    die unless $other->isa('Scaga::Path');

    die "LHS must be a PPath" unless @{$self->{ppaths}} == 1;

    my $n = scalar(@{$self->{ppaths}->[0]->{patterns}});

    for my $i (0 .. $n) {
        for my $j ($i+1 .. $n) {
            my $ppath = $self->{ppaths}->[0]->slice($i, $j - $i);

            my $subpath = bless { ppaths => [$ppath] }, 'Scaga::Path';

            if ($subpath->match($other)) {
                return [$i, $j];
            }
        }
    }

    return undef;
}

use Data::Dumper;

sub match {
    my ($self, $other) = @_;

    die unless $other->isa('Scaga::Path');

    die "LHS must be a PPath" unless @{$self->{ppaths}} == 1;

#    warn Dumper($self);
#    warn Dumper($other);
    if (@{$other->{ppaths}} == 1) {
        return $self->{ppaths}->[0]->match($other->{ppaths}->[0]);
    }

    if (@{$other->{ppaths}} == 2) {
        my $n = @{$self->{ppaths}->[0]->{patterns}};
        for my $i (0 .. @{$self->{ppaths}->[0]->{patterns}}) {
            for my $j ($i + 1 .. $n) {
                my $lslice = $self->{ppaths}->[0]->slice(0, $i);
                my $rslice = $self->{ppaths}->[0]->slice($j, $n - $j);

                if ($lslice->match($other->{ppaths}->[0]) &&
                    $rslice->match($other->{ppaths}->[1])) {
                    return 1;
                }
            }
        }
    }

    return 0;
}

package Scaga::Rule;

sub repr {
    my ($self) = @_;

    return $self->{in}->repr . " => " . $self->{out}->repr;
}

sub new {
    my ($class, $string) = @_;

    if ($string =~ /^(.*) => (.*)$/) {
        my ($in, $out) = ($1, $2);

        my $self = bless { in => Scaga::Path->new($in),
                           out => Scaga::Path->new($out) }, $class;

        return $self;
    }

    if ($string =~ /^(.*) =>$/) {
        my ($in, $out) = ($1, '');

        my $self = bless { in => Scaga::Path->new($in),
                           out => Scaga::Path->new($out) }, $class;

        return $self;
    }

    die "$string";
    return undef;
}

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
