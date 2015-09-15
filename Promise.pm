package Promise;
use Data::Dumper;
use strict;

$Data::Dumper::Deparse = 1;
$Data::Dumper::Sortkeys = 1;

%Promise::unforced = ();
$Promise::nforced = 0;

sub promise {
    my ($class, $value) = @_;

    if (defined $value and $value->UNIVERSAL::isa('Promise')) {
        return $value;
    } else {
        return Promise::Forced->new($value);
    }
}

sub new {
    my ($class, %h) = @_;
    my $self = bless {}, $class;

    $self->{force1} = $h{force1} if exists $h{force1};
    $self->{check1} = $h{check1} if exists $h{check1};
    $self->{finish} = $h{finish} if exists $h{finish};

    $self->{forced} = $self unless $self->{force1};

    if (!exists $self->{forced}) {
        $Promise::unforced{$self} = $self;
    }

    return $self;
}

sub finish {
    my ($self) = @_;

    return $self->{finish}->() if exists $self->{finish};
}

sub check1 {
    my ($self) = @_;

    return $self->{check1}->() if exists $self->{check1};

    return 1;
}

sub forced {
    return 0;
}

sub force1 {
    my ($self) = @_;

    # warn "forcing " . Dumper($self);

    return $self->{forced} if exists $self->{forced};

    die unless $self->{force1};

    $self->{forced} = $self->{force1}->() // $self;

    delete $Promise::unforced{$self};
    $Promise::nforced++;

    $self->finish;

    return $self->{forced};
}

sub check_all {
    my @promises = values %Promise::unforced;
    my $retval = 1;

    for my $promise (@promises) {
        if ($promise->check1) {
            $retval = 0;
            $promise->force1;
        }
    }

    warn "forced " . $Promise::nforced . " promises, " . scalar(keys(%Promise::unforced)) . " unforced";

    return $retval;
}

sub force {
    my ($self) = @_;

    while (!$self->forced) {
        $self = $self->force1;
    }

    return $self;
}

sub strip {
    my ($self, $pattern) = @_;

    $pattern = Promise->promise($pattern);

    my $force1;
    my $check1;
    my $finish;

    $force1 = sub {
        $self = $self->force1;
        return unless $self->forced;
        $pattern = $pattern->force1;
        return unless $pattern->forced;

        my $value = $self->value;
        my $vpattern = $pattern->value;

        return Promise::Forced->new(undef)
            unless $value =~ s/$vpattern//ms;

        return Promise::Forced->new($value);
    };

    $check1 = sub {
        return $self->check1 && $pattern->check1;
    };

    $finish = sub {
        undef $force1;
        undef $check1;
        undef $finish;
    };

    return Promise->new(force1 => $force1,
                        check1 => $check1,
                        finish => $finish);
}

package Promise::Apply;
use parent -norequire, 'Promise';

sub new {
    my ($class, $f, @args) = @_;

    my $n = scalar(@args);

    for my $i (0 .. $n-1) {
        $args[$i] = Promise->promise($args[$i]);
    }

    my $force1;
    my $check1;
    my $finish;

    $force1 = sub {
        for my $i (0 .. $n-1) {
            $args[$i] = $args[$i]->force;
            return unless $args[$i]->forced;
        }

        my $value = $f->(map { $_->value } @args);

        return Promise->promise($value);
    };

    $check1 = sub {
        for my $i (0 .. $n-1) {
            return 0 unless $args[$i]->check1;
        }

        return 1;
    };

    $finish = sub {
        undef $force1;
        undef $check1;
        undef $finish;
    };

    return Promise->new(force1 => $force1,
                        check1 => $check1,
                        finish => $finish);
}

package Promise::FixPoint;

sub new {
    my ($class, $f, $arg, @args) = @_;
    my $n = scalar(@args);

    die unless defined $arg;

    my $value;
    my $force1;
    my $check1;
    my $finish;

    $force1 = sub {
        $arg = $arg->force1;
        return unless $arg->forced;

        for my $i (0 .. $n-1) {
            $args[$i] = $args[$i]->force;
            return unless $args[$i]->forced;
        }

        unless (defined $value) {
            $value = $f->($arg->value, map { $_->value } @args);
        }

        $value = $value->force1;
        return unless $value->force;

        if ($arg->value eq $value->value) {
            return Promise->promise($value->value);
        } else {
#            warn "recursing fp, " . $arg->value . " ne " . $value->value;
            return Promise::FixPoint->new($f, $value, @args);
        }
    };

    $check1 = sub {
        while (!$arg->forced) {
            if (!$arg->check1) {
                return 0;
            }
            $arg = $arg->force1;
        }

        for my $i (0 .. $n-1) {
            while (!$args[$i]->forced) {
                if (!$args[$i]->check1) {
                    return 0;
                }
                $args[$i] = $args[$i]->force1;
            }
        }

        $value = $f->($arg->value, map { $_->value } @args) unless defined $value;

        while (!$value->forced) {
            if (!$value->check1) {
                return 0;
            }
            $value = $value->force1;
        }

        return 1;
    };

    $finish = sub {
        undef $value;
        undef $force1;
        undef $check1;
        undef $finish;
    };

    return Promise->new(force1 => $force1,
                        check1 => $check1,
                        finish => $finish);
}

package Promise::If;
use parent -norequire, 'Promise';

sub new {
    my ($class, $test, $value, $then, $else) = @_;

    my $force1;
    my $check1;
    my $finish;

    $force1 = sub {
        $value = $value->force1;
        return unless $value->forced;
        if ($test->($value->value)) {
            return $then;
        } else {
            return $else // Promise::Forced->new(undef);
        }
    };

    $check1 = sub {
        while (!$value->forced) {
            if (!$value->check1) {
                return 0;
            }
            $value = $value->force1;
        }

        if ($test->($value->value)) {
            while (!$then->forced) {
                if (!$then->check1) {
                    return 0;
                }
                $then = $then->force1;
            }
        } elsif (defined($else)) {
            while (!$else->forced) {
                if (!$else->check1) {
                    return 0;
                }
                $else = $else->check1;
            }
        }

        return 1;
    };

    $finish = sub {
        undef $force1;
        undef $check1;
        undef $finish;
    };

    return Promise->new(force1 => $force1,
                        check1 => $check1,
                        finish => $finish);
}

package Promise::Forced;
use parent -norequire, 'Promise';

sub new {
    my ($class, $value, %h) = @_;

    delete $h{force1};
    delete $h{check1};

    my $ret = $class->SUPER::new(%h);

    $ret->{value} = $value;

    return $ret;
}

sub forced {
    return 1;
}

sub force1 {
    my ($self) = @_;

    return $self;
}

sub value {
    my ($self) = @_;

    return $self->{value};
}

1;
