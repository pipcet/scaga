#!/usr/bin/perl

# run like this:

# for a in *.gimple; do echo "$a"; time perl ~/git/scaga/scaga-calls-async.pl < $a > /dev/null && mv calls.dump.pl $a.calls.dump.pl; done

use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp/;
use Getopt::Long;
use strict;

my $DEBUG = 0;
my @cmd = ("/usr/bin/gdb");

my $do_symbols;

GetOptions("symbols=i" => \$do_symbols);


my $in;
my $out = '';
my $in2;

my $h = start(\@cmd, \$out, \$in, '2>', \$in2);

my $outlog;
open $outlog, ">outlog.txt" or die;

my $inlog;
open $inlog, ">inlog.txt" or die;

my $comblog;
open $comblog, ">comblog.txt" or die;

select( ( select( $out ), $|=1 )[ 0 ] );

select( ( select( $comblog ), $|=1 )[ 0 ] );

my $cache = { };
my @handlers;
my @calls;
my $caller;
my $component;
my %functions;
my %components;
my %types;
my %lineno;

sub runcmd {
    my ($cmd) = @_;

    chomp $cmd;
    $cmd .= "\n" if $cmd ne "";

    return $cache->{$cmd} if defined $cache->{$cmd};

    print STDERR "cmd: $cmd" if $DEBUG;
    print $outlog $cmd;
    print $comblog $cmd;
    $out .= $cmd;
    chomp $cmd;

    my $ret = undef;
    my $rret = \$ret;

    push @handlers, sub {
        print STDERR "in: $in\n" if $DEBUG;
        die $in unless $in =~ /\A(.*?)\n----\n\n(.*)/ms;
        my ($retval, $rest) = ($1, $2);
        chomp $retval;
        print $inlog "$cmd => $retval\n\n\n";
        print STDERR "read: $retval\n" if $DEBUG;
        $$rret = $retval;
        $in = $rest;
    };
    warn scalar(@handlers) . " handlers" if $DEBUG;
    warn $in2 if $in2;
    $in2 = "";

    while(@handlers > 1000) {
        pump();
    }

    pump_nb();

    chomp $cmd;
    $cmd .= "\n" if $cmd ne "";

    return $cache->{$cmd} = sub { while (!defined($ret)) { pump(); }; return $ret; };
}

sub pump_nb {
    $h->pump_nb;

    while ($in =~ /\n----\n\n/ms && @handlers) {
        my $handler = shift @handlers;
        $handler->();
    }
}

sub pump {
    while ($in =~ /\n----\n\n/ms && @handlers) {
        my $handler = shift @handlers;
        $handler->();
    }
    if (@handlers) {
        $h->pump;
    } else {
        $h->pump_nb;
    }
}

sub sync {
    while (@handlers) {
        pump;
        warn scalar(@handlers) . " handlers" if $DEBUG;
    }
}

# $ofh = select STDOUT; $| = 1; select $ofh;

pump_nb;
sleep(1);
$in = "";
runcmd("\nset prompt \\n----\\n\\n\n\n\n");
runcmd("");
$cache = { };
runcmd("");
runcmd("echo test");
runcmd "file emacs";
runcmd "start";
runcmd "set width unlimited";
sync;

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

sub p {
    my ($rip, $expr) = @_;

    die unless defined $rip;

    return sub {
        if (ref $rip) {
            $rip = $rip->();

            return p($rip, $expr);
        }

        my $fret = runcmd ("if 1\np \$rip = $rip\np $expr\nend");

        return fstrip($fret, '.*\$[0-9]* = ');
    };
}

my $pwd;
chomp($pwd = `/bin/pwd`);

sub register_call {
    my ($caller, $callee, $file, $line, $col, $component, $inexpr) = @_;

    push @calls, { caller => $caller, callee => $callee, file => $file, line => $line, col => $col };

    my $call = $calls[$#calls];
    my $rip = file_line_col_to_rip($file, $line, $col);

    my $caller_type = function_type($rip, $caller);
    my $callee_type = function_type($rip, $callee);
    my $caller_id = p($rip, $caller);
    my $callee_id = p($rip, $callee);
    my $codeline;

    $call->{caller_type} = $caller_type;
    $call->{callee_type} = $callee_type;
    $call->{codeline} = $codeline = grab_line($file, $line);
    $call->{caller_id} = $caller_id;
    $call->{callee_id} = $callee_id;
    $call->{component} = $component;
    $call->{inexpr} = data_type($rip, $inexpr) if $inexpr ne "";

#    print "$caller_type $caller = $caller_id calls $callee_type $callee = $callee_id at $file:$line: $codeline\n";

    return $calls[$#calls];
}

sub register_function {
    my ($function, $file, $line, $col, $component) = @_;

    return register_call ($function, $function, $file, $line, $col, $component);
}

sub register_suggested_type {
    my ($function, $file, $line, $col, $component, $inexpr) = @_;

    return register_call ($function, $function, $file, $line, $col, $component, $inexpr);
}

if ($do_symbols) {
    my $fh;
    open $fh, "nm emacs| cut -c 20- |" or die;
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
                $done = 0;
            }
            if ($output =~ s/0x[0-9a-f]+ \<(.*?)\>$//ms) {
                my ($function) = ($1);
                for my $component (@{$symbol_components{$symbol}}) {
                    register_suggested_type($function, "...", 0, 0, $component, $symbol);
                }
            }

            delete $symbols{$symbol};
        }
    }
}

while (<>) {
    if (/^([a-zA-Z_].*?) \(/) {
        $caller = $1;
        $functions{$caller} = $1;
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
        $types{$id} = $type;
    }
    if (/^ *\[(.*?):(.*?):(.*?)\] gimple_assign <component_ref, ([a-zA-Z_.][a-zA-Z0-9_.]*), (\[(.*?):(.*?):(.*?)\] )*(\**([a-zA-Z_.][\[\]a-zA-Z0-9_.]*)(->|\.)([a-zA-Z_.][a-zA-Z0-9_.]*))[,>]/) {
        my ($file, $line, $col, $assignee, $linespec, $file1, $line1, $col1, $expr) =
            ($1, $2, $3, $4, $5, $6, $7, $8, $9);
        my ($inexpr, $comp);

        if ($expr =~ /->/) {
            $expr =~ /^(.*?)->(.*)$/;
            ($inexpr, $comp) = ($1, $2);
        } else {
            $expr =~ /^(.*)\.([^.]*)$/;
            ($inexpr, $comp) = ($1, $2);
        }

        if (defined ($comp)) {
            $components{$assignee} = [$comp, $inexpr];
        }
    }
    if (/^ *\[(.*?):(.*?):(.*?)\] gimple_assign <addr_expr, (\[.*?:.*?:.*?\] )*\**([a-zA-Z_.][\[\]a-zA-Z0-9_.]*)(->|\.)([a-zA-Z_.][a-zA-Z0-9_.]*), (\[.*?:.*?:.*?\] )*([a-zA-Z_.][a-zA-Z0-9_.]*)[,>]/) {
        my ($file, $line, $col, $dummy1, $expr, $op, $component, $dummy2, $value) =
            ($1, $2, $3, $4, $5, $6, $7, $8, $9);

        register_suggested_type($value, $file, $line, $col, $component);
    }
    if (/^ *\[(.*?):(.*?):(.*?)\] gimple_call <([a-zA-Z_.][a-zA-Z0-9_.]+)[,>]/) {
        my ($file, $line, $col, $callee) = ($1, $2, $3, $4);
        my ($comp, $inexpr);

        if (($callee =~ /^_/ or $callee =~ /\./)&& $types{$callee}) {
            ($comp, $inexpr) = @{$components{$callee}};
            $callee = $types{$callee};
        }

        $functions{$callee} = 1;
        $lineno{$callee} = $file . ":" . $line;

        # print $caller . " calls " . $callee . "\n";
        # $callers{$callee}{$caller} = 1;
        register_call($caller, $callee, $file, $line, $col, $comp, $inexpr);
    }
    if (/>>\[(.*?):([0-9]*?):([0-9*])\] gimple_bind/) {
        my ($file, $line, $col) = ($1, $2, $3);

        register_function($caller, $file, $line, $col, $component);
    }
}

sub file_line_col_to_rip {
    my ($file, $line, $col) = @_;

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
            return fstrip(runcmd("if 1\np \$rip=${rip}\nwhatis $function\nend"), '.*\$[0-9]* = ');
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

    return fstrip($fret1, '.*type = ');
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

my $notdone = 1;
while ($notdone) {
    $notdone = 0;
    for my $call (@calls) {
        for my $key (keys %$call) {
            if (ref $call->{$key}) {
                $call->{$key} = $call->{$key}->();
                $notdone = 1;
            }
        }
    }
    sync;
}

use Data::Dumper;

print Dumper(\@calls);

exit 0;
