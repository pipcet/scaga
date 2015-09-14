#!/usr/bin/perl

# run like this:

# for a in *.gimple; do echo "$a"; time perl ~/git/scaga/scaga-calls-async.pl < $a > /dev/null && mv calls.dump.pl $a.calls.dump.pl; done

use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp/;
use Getopt::Long;
use strict;
use Scaga;

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

    my $retval;
    $retval = sub { while (!defined($ret)) { $self->pump(); }; undef $retval; return $ret; };

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

sub pump_nb {
    for my $gdbb (@gdbb) {
        $gdbb->pump_nb;
    }

    print STDERR join(" ", map { $_->handlers } @gdbb) . "\n";
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

sub fstrip {
    my ($f, @patterns) = @_;

    return sub {
        if (ref $f) {
            return fstrip($f->(), @patterns);
        }

        my $ret = $f;

        for my $pattern (@patterns) {
            while ($ret =~ s/$pattern//msg) { }
        }

        die unless defined $ret;

        return $ret;
    };
}

sub fstrip2 {
    my ($f, @patterns) = @_;

    return sub {
        if (ref $f) {
            return fstrip2($f->(), @patterns);
        }

        my $ret = $f;

        for my $pattern (@patterns) {
            unless ($ret =~ s/$pattern//msg) {
                return undef;
            }
        }

        die unless defined $ret;

        return $ret;
    };
}

sub p {
    my ($rip, $expr) = @_;

    die unless defined $rip;

    return sub {
        if (ref $rip) {
            $rip = $rip->();

            return p($rip, $expr);
        }

        my $fret = runcmd ("if 1\np \$rip = $rip\np $expr\nend");

        return fstrip2($fret, '.*\$[0-9]* = ');
    };
}

sub data_value {
    my ($rip, $expr) = @_;

    return p($rip, $expr);
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
    my %symbol_components;
    map { chomp } @symbols;
    close $fh;

    for my $symbol (@symbols) {
        $symbols{$symbol} = p("main", $symbol);
    }

    my $done = 0;

    while(!$done) {
        $done = 1;
        for my $symbol (@symbols) {
            if (!exists $symbols{$symbol}) {
                next;
            }
            if (ref $symbols{$symbol}) {
                $symbols{$symbol} = $symbols{$symbol}->();
                $done = 0;
                next;
            }

            my $output = $symbols{$symbol};
            my $function;
            my $component;

            while ($output =~ s/([a-zA-Z0-9_][a-zA-Z0-9_]*) = //ms) {
                push @symbols, "$symbol\.$1";
                $symbols{"$symbol\.$1"} = p("main", "$symbol\.$1");
                push @{$symbol_components{$symbol}}, $1;
                push @{$symbol_components{$symbol.".".$1}}, $1;
                $done = 0;
            }
            if ($output =~ s/0x[0-9a-f]+ \<(.*?)\>$//ms) {
                my ($function) = ($1);
                for my $component (@{$symbol_components{$symbol}}) {
                    e('type', [[Scaga::Component::Component->new($component)], [Scaga::Component::Identifier->new($function)]]);
                }
            }

            delete $symbols{$symbol};
        }
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
            $intype = $types{$baseexpr}{base} ? $types{$baseexpr}{base}->repr : data_type($rip, $baseexpr);
            ($inexpr, $comp) = ('*('.$baseexpr.')', $2);
        } else {
            $expr =~ /^(.*)\.([^.]*)$/;
            my $baseexpr = $1;
            $intype = $types{$baseexpr}{base} ? $types{$baseexpr}{base}->repr : data_type($rip, $baseexpr);
            ($inexpr, $comp) = ($baseexpr, $2);
        }

        if (defined ($comp)) {
            warn "inexpr $inexpr intype $intype";
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

sub flc_to_rip {
    my ($flc) = @_;
    my ($file, $line, $col) = split ":", $flc->{flc};

    my $fret2 = runcmd("info line $file:$line");

    return sub {
        my $fret = $fret2;
        while (ref $fret) {
            $fret = $fret->();
        }
        if ($fret =~ s/Line (.*?) of \"(.*?)\"( |\t|\n)*(is|starts) at address (.*?) <.*$/$5/msg) {
            die unless $1 eq $line;
            die unless $2 eq $file;
            return $fret;
        }

        return "main";

        die "no rip for $file:$line: $fret";
        return undef;
    };
}

sub function_type {
    my ($rip, $function) = @_;

    if (ref $rip) {
        $rip = $rip->();

        return function_type($rip, $function);
    }

    my $fret1 = runcmd "if 1\np \$rip=${rip}\nwhatis (${function})0\nend";

    return sub {
        my $ret1 = $fret1->();

        while (ref $ret1) {
            $ret1 = $ret1->();
        }

        if ($ret1 =~ /type = /ms) {
            return fstrip2(runcmd("if 1\np \$rip=${rip}\nwhatis $function\nend"), '.*\$[0-9]* = ');
        } else {
            return fstrip(runcmd("if 1\np \$rip=${rip}\nwhatis \&($function)\nend"), '.*type = ', '.*\$[0-9]*.*');
        }
    };
}

sub data_type {
    my ($rip, $function) = @_;

    if (ref $rip) {
        $rip = $rip->();

        return data_type($rip, $function);
    }

    my $fret1 = runcmd "if 1\np \$rip=${rip}\nwhatis ${function}\nend";
    my $fret2 = fstrip2($fret1, '.*type = ');

    my $ret;
    $ret = sub {
        if (ref $fret2) {
            $fret2 = $fret2->();
            return $ret;
        }

        $ret = undef;

        if ($function eq $fret2) {
            return $function;
        } else {
            return data_type($rip, $fret2);
        }
    };
}

sub grab_line {
    my ($file, $line, $line2) = @_;
    $line2 = $line unless defined $line2;

    my $fret = runcmd "l ${pwd}/${file}:${line},${line2}";
    return sub {
        my $ret = $fret->();

        $ret =~ s/^[0-9]*[ \t]*//msg;

        if ($ret =~ /^[({]/) {
            my $fret2 = grab_line($file, $line-1, $line-1);
            return sub {
                my $ret2 = $fret2->();

                return $ret . $ret2;
            }
        }

        return $ret;
    };
}

use Data::Dumper;

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
