package Generator;
use Time::HiRes qw(time);
use strict;

sub new {
    my ($class, %p) = @_;

    my $self = bless {}, $class;

    for my $k (keys %p) {
        $self->{$k} = $p{$k};
    }

    $self->{out} = [];
    $self->{age} = time;

    return $self;
}

sub eof {
    my ($self, $eof) = @_;

    $self->{eof} = $eof if defined $eof;

    return (!@{$self->{out}}) && $self->{eof};
}

sub fetch {
    my ($self) = @_;

    while (1) {
        if (@{$self->{out}}) {
            return shift @{$self->{out}};
        }

        return if $self->eof;

        push @{$self->{out}}, $self->yield;
    }
}

sub yield {
    my ($self) = @_;

    $self->{yield}->($self);
}

sub abort {
    my ($self) = @_;

    $self->{abort}->($self);
    undef $self->{yield};
    undef $self->{abort};

    $self->eof(1);
}

1;
