#!/usr/bin/perl

# run like this:

# for a in *.gimple; do echo "$a"; time perl ~/git/scaga/scaga-calls-async.pl < $a > /dev/null && mv calls.dump.pl $a.calls.dump.pl; done

use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp/;

my $DEBUG = 0;
my @cmd = ("/usr/bin/gdb");

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
    $h->pump_nb;
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

sub register_call {
    my ($caller, $callee, $file, $line, $col, $component) = @_;

    push @calls, [ $caller, $callee, $file, $line, $col ];

    my $call = $calls[$#calls];
    my $rip = file_line_col_to_rip($file, $line, $col);

    my $caller_type = function_type($rip, $caller);
    my $callee_type = function_type($rip, $callee);
    my $caller_id = p($rip, $caller);
    my $callee_id = p($rip, $callee);
    my $codeline;

    $call->[5] = $caller_type;
    $call->[6] = $callee_type;
    $call->[7] = $codeline = grab_line($file, $line);
    $call->[8] = $caller_id;
    $call->[9] = $callee_id;
    $call->[10] = $component;

    print "$caller_type $caller = $caller_id calls $callee_type $callee = $callee_id at $file:$line: $codeline\n";

    return $calls[$#calls];
}

sub register_function {
    my ($function, $file, $line, $col) = @_;

    return register_call ($function, $function, $file, $line, $col);
}

while (<>) {
    if (/^([a-zA-Z_].*?) \(/) {
        $caller = $1;
        $functions{$caller} = $1;
    }
    if (/^ *(.*?) ([a-zA-Z_.][a-zA-Z0-9_.]+);$/) {
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
    if (/^ *\[(.*?):(.*?):(.*?)\] gimple_assign <component_ref, ([a-zA-Z_.][a-zA-Z0-9_.]+), \[(.*?):(.*?):(.*?)\] (([a-zA-Z_.][a-zA-Z0-9_.]+)(->|\.)([a-zA-Z_.][a-zA-Z0-9_.]+))[,>]/) {
        my ($file, $line, $col, $assignee, $file1, $line1, $col1, $expr) =
            ($1, $2, $3, $4, $5, $6, $7, $8);
        my ($inexpr, $comp);

        if ($expr =~ /->/) {
            $expr =~ /^(.*?)->(.*)$/;
            ($inexpr, $comp) = ($1, $2);
        } else {
            $expr =~ /^(([0-9A-Za-z_]*)(\.[0-9]*)?)\.(.*)$/;
            ($inexpr, $comp) = ($1, $2);
        }

        if (defined ($comp)) {
            $components{$assignee} = $comp;
        }
    }
    if (/^ *\[(.*?):(.*?):(.*?)\] gimple_call <([a-zA-Z_.][a-zA-Z0-9_.]+)[,>]/) {
        my ($file, $line, $col, $callee) = ($1, $2, $3, $4);
        my $comp;

        if (($callee =~ /^_/ or $callee =~ /\./)&& $types{$callee}) {
            $comp = $components{$callee};
            $callee = $types{$callee};
        }

        $functions{$callee} = 1;
        $lineno{$callee} = $file . ":" . $line;

        # print $caller . " calls " . $callee . "\n";
        # $callers{$callee}{$caller} = 1;
        $callees{$caller}{$callee} = "$caller > $callee";
        $call = register_call($caller, $callee, $file, $line, $col, $comp);
    }
    if (/>>\[(.*?):([0-9]*?):([0-9*])\] gimple_bind/) {
        my ($file, $line, $col) = ($1, $2, $3);

        register_function($caller, $file, $line, $col);
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

sub grab_line {
    my ($file, $line, $line2) = @_;
    $line2 = $line unless defined $line2;

    my $fret = runcmd "l ./$file:$line,$line2";
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
        for my $i (0 .. $#$call) {
            if (ref $call->[$i]) {
                $call->[$i] = $call->[$i]->();
                $notdone = 1;
            }
        }
    }
    sync;
}


for my $call (@calls) {
    $call->[7] => s/.*\$[0-9]*//g;
}

use Data::Dumper;

my $fh;
open $fh, ">calls.dump.pl" or die;

print $fh Dumper(\@calls);

close $fh;

exit 0;
