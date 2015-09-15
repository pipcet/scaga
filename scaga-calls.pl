#!/usr/bin/perl

# run like this:

# for a in *.gimple; do echo "$a"; time perl ~/git/scaga/scaga-calls-async.pl < $a > /dev/null && mv calls.dump.pl $a.calls.dump.pl; done

use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp/;
use Getopt::Long;
use strict;
use Scaga;
use Promise;

our $DEBUG = 0;
my @cmd = ("/usr/bin/gdb");

my $do_symbols;

GetOptions("symbols=i" => \$do_symbols);


package GDBB;
use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use strict;

sub runcmd {
    my ($self, $cmd, $nocache) = @_;

    chomp $cmd;
    $cmd .= "\n" if $cmd ne "";

    unless ($nocache) {
        return $self->{cache}->{$cmd} if defined $self->{cache}->{$cmd};
    }

    print STDERR "cmd: $cmd" if $DEBUG;
    $self->{outlog}->print($cmd);
    $self->{comblog}->print($cmd);
    $self->{out} .= $cmd;
    chomp $cmd;

    my $ret = undef;
    my $rret = \$ret;

    push @{$self->{handlers}}, sub {
        print STDERR "in: " . $self->in . "\n" if $DEBUG;
        die $self->in unless $self->in =~ /\A(.*?)\n----\n\n(.*)/ms;
        my ($retval, $rest) = ($1, $2);
        chomp $retval;
        $self->{inlog}->print("$cmd => $retval\n\n\n");
        print STDERR "read: $retval\n" if $DEBUG;
        $$rret = $retval;
        $self->{in} = $rest;
    };
    warn $self->handlers . " handlers" if $DEBUG;
    # warn $self->{in2} if $self->{in2};
    $self->{in2} = "";

    $self->pump_nb();

    chomp $cmd;
    $cmd .= "\n" if $cmd ne "";

    my $force1;
    my $check1;
    my $finish;

    $force1 = sub {
        while (!defined($ret)) {
            $self->pump();
        }

        return Promise::Forced->new($ret);
    };

    $check1 = sub {
        if (!defined($ret)) {
            $self->pump_nb();
        }

        return defined($ret);
    };

    $finish = sub {
        undef $force1;
        undef $check1;
        undef $finish;
    };

    my $retval = Promise->new(force1 => $force1,
                              check1 => $check1,
                              finish => $finish);

    unless($nocache) {
        $self->{cache}->{$cmd} = $retval;
    }

    return $retval;
}

sub handlers {
    my ($self) = @_;

    return scalar(@{$self->{handlers}});
}

sub handler {
    my ($self) = @_;

    return shift @{$self->{handlers}};
}

sub in {
    my ($self) = @_;

    return $self->{in};
}

sub pump_nb {
    my ($self) = @_;
    $self->{h}->pump_nb;

    while ($self->in =~ /\n----\n\n/ms && $self->handlers) {
        my $handler = $self->handler;
        $handler->();
    }
}

sub pump {
    my ($self) = @_;
    while ($self->in =~ /\n----\n\n/ms && $self->handlers) {
        my $handler = $self->handler;
        $handler->();
    }
    if ($self->handlers) {
        $self->{h}->pump;
    } else {
        $self->{h}->pump_nb;
    }
}

sub sync {
    my ($self) = @_;
    while ($self->handlers) {
        $self->pump;
        warn $self->handlers . " handlers" if $DEBUG;
    }
}

sub new {
    my ($class, $cmd, %h) = @_;
    my $self = bless {}, $class;

    $self->{cache} = $h{cache} // {};
    $self->{handlers} = [];
    $self->{out} = "";
    $self->{in} = "";
    $self->{in2} = "";
    $self->{h} = start($cmd, \$self->{out}, \$self->{in}, '2>', \$self->{in2});

    my $outlog;
    my $inlog;
    my $comblog;

    $outlog = $h{outlog} or open $outlog, ">outlog.txt" or die;
    $inlog = $h{inlog} or open $inlog, ">inlog.txt" or die;
    $comblog = $h{comblog} or open $comblog, ">comblog.txt" or die;

    $self->{outlog} = $outlog;
    $self->{inlog} = $inlog;
    $self->{comblog} = $comblog;

    $self->pump_nb;
    sleep(1);
    $self->{in} = "";
    $self->runcmd("\nset prompt \\n----\\n\\n\n\n\n", 1);
    $self->runcmd("", 1);
    $self->runcmd("", 1);
    $self->runcmd("echo test", 1);
    $self->runcmd("file emacs", 1);
    $self->runcmd("start", 1);
    $self->runcmd("set width unlimited", 1);
    $self->sync;

    return $self;
}

package main;

my $n_gdbb = 3;
my @gdbb;

push @gdbb, GDBB->new(\@cmd);
while (@gdbb < $n_gdbb) {
    push @gdbb, GDBB->new(\@cmd, cache => $gdbb[0]->{cache});
}

sub gdbb {
    my @l = sort { $a->handlers <=> $b->handlers } @gdbb;

    return $l[0];
}

sub gdbb9 {
    my @l = sort { $a->handlers <=> $b->handlers } @gdbb;

    return $l[$#l];
}

my $last_time;

sub pump_nb {
    for my $gdbb (@gdbb) {
        $gdbb->pump_nb;
    }

    if (time() != $last_time) {
        warn join(" ", map { $_->handlers } @gdbb) . "\n";
        $last_time = time();
    }
}

sub runcmd($) {
    my $ret = gdbb()->runcmd(@_);

    pump_nb();
    my $gdbb9 = gdbb9();
    while($gdbb9->handlers > 1000) {
        $gdbb9->pump();
        $gdbb9 = gdbb9();
        pump_nb();
    }

    return $ret;
}

my @calls;
my $caller;
my $component;
my %components;
my %types;

# this parses the output of gcc -fdump-tree-gimple-vops-verbose-raw-lineno
# (.gimple)

sub p {
    my ($rip, $expr) = @_;

    die unless defined $rip;

    $rip = Promise->promise($rip);
    $expr = Promise->promise($expr);

    my $force1;
    my $check1;
    my $finish;

    $force1 = sub {
        $rip = $rip->force1;
        return unless $rip->forced;
        $expr = $expr->force1;
        return unless $expr->forced;

        my $p = runcmd("if 1\np \$rip = " . $rip->value . "\np " . $expr->value . "\nend");

        return $p->strip('.*\$[0-9]* = ');
    };

    $check1 = sub {
        while (!$rip->forced) {
            if (!$rip->check1) {
                return 0;
            }
            $rip = $rip->force1;
        }

        while (!$expr->forced) {
            if (!$expr->check1) {
                return 0;
            }
            $expr = $expr->force1;
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

sub data_value {
    my ($rip, $expr) = @_;

    return p($rip, $expr);
}

sub p2 {
    my ($rip, $expr) = @_;

    die unless defined $rip;

    $rip = Promise->promise($rip);
    $expr = Promise->promise($expr);

    my $f = sub {
        my ($rip, $expr) = @_;
        my $p = runcmd("if 1\np \$rip = $rip\np $expr\nend");

        return $p;
    };

    return Promise::Apply->new($f, $rip, $expr)->strip('.*\$[0-9]* = ');
}

sub function_type {
    my ($rip, $expr) = @_;

    die unless defined $rip;

    $rip = Promise->promise($rip);
    $expr = Promise->promise($expr);

    my $f = sub {
        my ($rip, $expr) = @_;
        my $p = runcmd("if 1\np \$rip = $rip\nwhatis ($expr)0\nend");

        return $p;
    };

    my $f2 = sub {
        my ($rip, $ret1, $expr) = @_;

        if ($ret1 =~ /type = /ms) {
            return runcmd("if 1\np \$rip=${rip}\nwhatis $expr\nend")->strip('.*type = ');
        } else {
            return runcmd("if 1\np \$rip=${rip}\nwhatis \&($expr)\nend")->strip('.*type = ');
        }
    };

    return Promise::Apply->new($f2, $rip, Promise::Apply->new($f, $rip, $expr), $expr);
}

sub data_type {
    my ($rip, $function) = @_;

    die unless defined $rip;

    $rip = Promise->promise($rip);

    my $f = sub {
        my ($expr, $rip) = @_;

        return Promise->promise(undef) unless defined($expr);

        return runcmd("if 1\np \$rip=${rip}\nwhatis ${expr}\nend")->strip('.*type = ');
    };

    return Promise::FixPoint->new($f, $function, $rip);
}

sub flc_to_rip {
    my ($flc) = @_;
    my ($file, $line, $col) = split ":", $flc->{flc};

    my $f = sub {
        my ($info) = @_;

        if ($info =~ s/Line (.*?) of \"(.*?)\"( |\t|\n)*(is|starts) at address (.*?) <.*$/$5/msg) {
            die unless $1 eq $line;
            die unless $2 eq $file;
            return $info;
        }

        return "main";
    };

    return Promise::Apply->new($f, runcmd("info line $file:$line"));
}

my $pwd;
chomp($pwd = `/bin/pwd`);

my @e;
sub e {
    my ($kind, $path) = @_;

    push @e, [$kind, $path];
}

sub grab_codeline {
    return "";
}

if ($do_symbols) {
    my $fh;
    open $fh, "nm /usr/local/bin/emacs| cut -c 20- |" or die;
    my @symbols = <$fh>;
    my %symbols;
    @symbols = sort @symbols;
    my %symbol_components;
    map { chomp } @symbols;
    close $fh;

    for my $symbol (@symbols) {
        $symbols{$symbol} = p("main", $symbol);
    }

    while (!Promise->check_all) {
        sleep(1);
    }

    while (my $symbol = shift @symbols) {
        if (!$symbols{$symbol}->check1) {
            Promise->check_all;
            push @symbols, $symbol;
            next;
        } elsif (!$symbols{$symbol}->forced) {
            $symbols{$symbol} = $symbols{$symbol}->force1;
            Promise->check_all;
            push @symbols, $symbol;
            next;
        }

        my $output = $symbols{$symbol}->value;
        my $function;
        my $component;

        $symbol_components{$symbol} //= [];
        while ($output =~ s/([a-zA-Z0-9_][a-zA-Z0-9_]*) = //ms) {
            my $component = $1;
            my $o = [$symbol,
                     $component,
                     data_type("main", Promise->promise($symbol)),
                     data_type("main", Promise->promise("$symbol.$component"))];
            unless ($symbols{"$symbol.$component"}) {
                push @symbols, "$symbol.$component";
                $symbols{"$symbol.$component"} = p("main", "$symbol.$component");
            }
            push @{$symbol_components{$symbol}}, $o;
            push @{$symbol_components{$symbol.".".$component}}, $o;
        }
    }

    while (!Promise->check_all) {
        sleep(1);
    }

    @symbols = sort keys %symbol_components;
    my @subs;

    while (my $symbol = shift @symbols) {
        if (!exists $symbol_components{$symbol}) {
            next;
        }

        my $output = $symbols{$symbol}->value;
        my $function;
        my $component;

        if ($output =~ s/0x[0-9a-f]+ \<(.*?)\>$//ms) {
            my ($function) = ($1);
            for my $o (@{$symbol_components{$symbol}}) {
                my $symbol = $o->[0];
                my $component = $o->[1];
                my $symbol_type = $o->[2];
                my $component_type = $o->[3];
                my $function_type = function_type($function);
                my $function_value = data_value($function);

                push @subs, sub {
                    $symbol_type = $symbol_type->force->value;
                    $component_type = $component_type->force->value;
                    $function_type = $function_type->force->value;
                    $function_value = $function_value->force->value;

                    if (1 or ($component_type eq $function_type)) {
                        e('type', [[Scaga::Component::Component->new($component),
                                    Scaga::Component::Intype->new($symbol_type)],
                                   [Scaga::Component::Identifier->new($function),
                                    Scaga::Component::Value->new($function_value)]]);
                    }
                };
            }
        }
    }

    while (!Promise->check_all) {
        sleep(1);
    }

    for my $sub (@subs) {
        $sub->();
    }
}

while (<>) {
    if (/^([a-zA-Z_].*?) \(/) {
        $caller = Scaga::Component::Identifier->new($1);

    }
    if (/^ *(.*?) ([a-zA-Z_.][a-zA-Z0-9_.]*);$/ or
        />>>, (.*?) ([a-zA-Z_.][a-zA-Z0-9_.]*)$/) {
        my $id = $2;
        my $type = $1;

        while ($type =~ s/<[^>]*>//g) { }
        $type =~ s/struct GtkWidget/GtkWidget/g; # gimple is confused by typedefs.
        $type =~ s/prop_handled/enum prop_handled/g;
        $type =~ s/scroll_bar_part/enum scroll_bar_part/g;
        $type =~ s/glyph_row_area/enum glyph_row_area/g;
        $type =~ s/([^a-zA-Z_0-9])(text_cursor_kinds|xembed_message|xembed_info|draw_glyphs_face|corners|named_merge_point_kind|font_property_index)/$1enum $2/g;
        $type =~ s/\(\*\) */\(\*\)/msg;
        $types{$id}{base} = Scaga::Component::Identifier->new($type);
    }
    if (/^ *\[(.*?:.*?:.*?)\] gimple_assign <component_ref, ([a-zA-Z_.][a-zA-Z0-9_.]*), (\[(.*?:.*?:.*?)\] )*(\**([a-zA-Z_.][\[\]a-zA-Z0-9_.]*)(->|\.)([a-zA-Z_.][a-zA-Z0-9_.]*))[,>]/) {
        my ($flc, $id, $linespec, $flc1, $expr) =
            (Scaga::Component::FLC->new($1), $2, $3, $4, $5);
        my $rip = flc_to_rip($flc);
        my ($inexpr, $comp);
        my $intype;

        if ($expr =~ /->/) {
            $expr =~ /^(.*?)->(.*)$/;
            my $baseexpr = $1;
            if ($types{$baseexpr}{base}) {
                $intype = Promise::Forced->new($types{$baseexpr}{base}->repr);
            } else {
                $intype = data_type($rip, $baseexpr);
            }
            ($inexpr, $comp) = ('*('.$baseexpr.')', $2);
        } else {
            $expr =~ /^(.*)\.([^.]*)$/;
            my $baseexpr = $1;
            if ($types{$baseexpr}{base}) {
                $intype = Promise::Forced->new($types{$baseexpr}{base}->repr);
            } else {
                $intype = data_type($rip, $baseexpr);
            }
            ($inexpr, $comp) = ($baseexpr, $2);
        }

        if (defined ($comp)) {
            $types{$id}{component} = Scaga::Component::Component->new($comp);
            $types{$id}{inexpr} = Scaga::Component::Intype->lazy($intype);
        }
    }
    if (/^ *\[(.*?:.*?:.*?)\] gimple_assign <addr_expr, (\[.*?:.*?:.*?\] )*\**([a-zA-Z_.][\[\]a-zA-Z0-9_.]*)(->|\.)([a-zA-Z_.][a-zA-Z0-9_.]*), (\[.*?:.*?:.*?\] )*([a-zA-Z_.][a-zA-Z0-9_.]*)[,>]/) {
        my $flc = Scaga::Component::FLC->new($1);
        my $rip = flc_to_rip($flc);
        my $type = Scaga::Component::Identifier->lazy(data_type($rip, $3.$4.$5));
        my $intype = Scaga::Component::Intype->lazy(data_type($rip, $3));
        my $component = Scaga::Component::Component->new($5);
        my $id = Scaga::Component::Identifier->new($7);
        my $value = Scaga::Component::Value->lazy(data_value($rip, $7));
        e('devirt', [[$type, $component, $intype], [$id, $value]]);
    }
    if (/^ *\[(.*?:.*?:.*?)\] gimple_assign <parm_decl, (\[.*?:.*?:.*?\] )*\**([a-zA-Z_.][\[\]a-zA-Z0-9_.]*)(->|\.)([a-zA-Z_.][a-zA-Z0-9_.]*), (\[.*?:.*?:.*?\] )*([a-zA-Z_.][a-zA-Z0-9_.]*)[,>]/) {
        my $flc = Scaga::Component::FLC->new($1);
        my $rip = flc_to_rip($flc);
        my $type = Scaga::Component::Identifier->lazy(data_type($rip, $3.$4.$5));
        my $intype = Scaga::Component::Intype->lazy(data_type($rip, $3));
        my $component = Scaga::Component::Component->new($5);
        e('virt', [[$type], [$type, $component, $intype]]);
    }
    if (/^ *\[(.*?:.*?:.*?)\] gimple_call <([a-zA-Z_.][a-zA-Z0-9_.]+)[,>]/) {
        my ($flc, $callee) = (Scaga::Component::FLC->new($1), Scaga::Component::Identifier->new($2));
        my $rip = flc_to_rip($flc);
        my $value = Scaga::Component::Value->lazy(data_value($rip, $caller->repr));
        my $callee_value = Scaga::Component::Value->lazy(data_value($rip, $callee->repr));
        my ($comp, $inexpr);

        if (($callee->repr =~ /^_/ or $callee->repr =~ /\./) && $types{$callee->repr}) {
            $comp = $types{$callee->repr}{component};
            $inexpr = $types{$callee->repr}{inexpr};
            $callee = $types{$callee->repr}{base};
        }

        e('call', [[$caller, $value, $flc], [$callee, $callee_value, $comp, $inexpr]]);
    }
    if (/>>\[(.*?:[0-9]*?:[0-9]*)\] gimple_bind/) {
        my $flc = Scaga::Component::FLC->new($1);
        my $home = Scaga::Component::Home->new($1);
        my $rip = flc_to_rip($flc);
        my $type = Scaga::Component::Identifier->lazy(function_type($rip, $caller->repr));
        my $value = Scaga::Component::Value->lazy(data_value($rip, $caller->repr));

        e('type', [[$type], [$caller, $value]]);
        e('home', [[$caller, $value], [$home]]);
    }
}

while (!Promise->check_all) {
    sleep(1);
}

my @paths;
for my $e (@e) {
    my $ekind = shift @$e;
    my @patterns;
    for my $epattern (@{$e->[0]}) {
        my @components;

        for my $component (@$epattern) {
            push @components, $component if defined $component;
        }

        push @patterns, \@components;
    }

    my $path = Scaga::Path->new([\@patterns]);

    print $ekind . " := " . $path->repr . "\n";
}

exit 0;
