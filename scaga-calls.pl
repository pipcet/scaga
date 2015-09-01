#!/usr/bin/perl

# run like this:

# for a in *.gimple; do echo "$a"; time perl ~/git/scaga/scaga-calls-async.pl < $a > /dev/null && mv calls.dump.pl $a.calls.dump.pl; done

use Carp::Always;
use IPC::Run qw/run new_chunker timeout start/;
use File::Slurp qw/slurp/;

my $DEBUG = 0;
my @cmd = ("/usr/bin/gdb");

my $in;
my $out;
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

    print STDERR "$cmd" if $DEBUG;
    print $outlog $cmd;
    print $comblog $cmd;
    $out .= $cmd;

    my $ret = undef;
    my $rret = \$ret;

    push @handlers, sub {
        print STDERR "in: $in\n" if $DEBUG;
        die unless $in =~ /\A(.*?)\(gdb\) *(.*)/msg;
        my ($retval, $rest) = ($1, $2);
        chomp $retval;
        print $inlog "$cmd => $retval\n\n\n";
        print STDERR "read: $retval\n" if $DEBUG;
        $$rret = $retval;
        $in = $rest;
    };
    warn scalar(@handlers) . " handlers" if $DEBUG;
    $in2 = "";

    while(@handlers > 1000) {
        pump();
    }

    pump_nb();

    return $cache->{$cmd} = sub { while (!defined($ret)) { pump(); }; return $ret; };
}

sub pump_nb {
    $h->pump_nb;

    while ($in =~ /\(gdb\) */ && @handlers) {
        my $handler = shift @handlers;
        $handler->();
    }
}

sub pump {
    while ($in =~ /\(gdb\) */ && @handlers) {
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

runcmd("");
runcmd("echo ");
runcmd "file emacs/src/emacs";
runcmd "start";
runcmd "set width unlimited";
sync;

# this parses the output of gcc -fdump-tree-gimple-vops-verbose-raw-lineno
# (.gimple)

sub fstrip {
    my ($f, $pattern) = @_;

    return sub {
        if (ref $f) {
            return fstrip($f->(), $pattern);
        }

        my $ret = $f;

        while ($ret =~ s/$pattern//msg) { }

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
    my ($caller, $callee, $file, $line, $col) = @_;

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
    if (/^ *\[(.*?):(.*?):(.*?)] gimple_call <([a-zA-Z_.][a-zA-Z0-9_.]+)[,>]/) {
        my ($file, $line, $col, $callee) = ($1, $2, $3, $4);

        if (($callee =~ /^_/ or $callee =~ /\./)&& $types{$callee}) {
            $callee = $types{$callee};
        }

        $functions{$callee} = 1;
        $lineno{$callee} = $file . ":" . $line;

        # print $caller . " calls " . $callee . "\n";
        # $callers{$callee}{$caller} = 1;
        $callees{$caller}{$callee} = "$caller > $callee";
        $call = register_call($caller, $callee, $file, $line, $col);
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

    my $fret1 = runcmd "if 1\np \$rip=${rip}\nwhatis \*(${function})\nend";

    return sub {
        my $ret1 = $fret1->();

        while (ref $ret1) {
            $ret1 = $ret1->();
        }

        if ($ret1 =~ /type = /msg) {
            return fstrip(runcmd("if 1\np \$rip=${rip}\nwhatis $function\nend"), ".*\\\$[0-9]\* = ");
        } else {
            return fstrip(runcmd("if 1\np \$rip=${rip}\nwhatis \&($function)\nend"), ".*type = ");
        }
    };
}

sub grab_line {
    my ($file, $line, $line2) = @_;
    $line2 = $line unless defined $line2;

    my $fret = runcmd "l $file:$line,$line2";
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

my @linenos = sort keys %lineno;
my $fh;
open $fh, ">tmp.gdb" or die;
print $fh "set width unlimited\n";
print $fh "set debug-file-directory /usr/lib/debug\n";
for my $function (@linenos) {
    print $fh "info line " . $lineno{$function} . "\n";
}
close $fh;
system("gdb --nx --nh --command=tmp.gdb emacs > tmp.gdbout");

sub get_rip {
    my ($fh, $lineno) = @_;
    my $type = "";

    my $line;

    while ($type eq "") {
    if (defined $cache) {
        $line = $cache;
    } else {
        die if eof($fh);
        $line = <$fh>;
        if ($line =~ s/^Line (.*?) of \"(.*?)\" (is|starts) at address (.*?) <.*$/$4/msg &&
            "$2:$1" eq $lineno{$lineno}) { # }$0/) {
            $type .= $line;
            last;
        }
    }
    }

    chomp $type;
    $type =~ s/\(/\(\*\) \(/ unless $type =~ /\(\*\)/;
    $type =~ s/\(\*\) */\(\*\) /g;

    return $type;
}

open $fh, "<tmp.gdbout" or die;
for my $lineno (@linenos) {
    ($rips{$lineno} = get_rip($fh, $lineno));
}


# function pointer parameters currently confuse the code.
my @functions = grep { $_ !~ /^(.*\..*|__builtin_.*|__atomic_.*)$/ } sort keys %functions;

# print "functions: " . join(", ", @functions);

# use gdb to determine the type of all functions (gimple doesn't store the return type);
my $fh;
open $fh, ">tmp.gdb" or die;
print $fh "set width unlimited\n";
print $fh "set debug-file-directory /usr/lib/debug\n";
print $fh "start\n";
for my $function (@functions) {
    if ($rips{$function}) {
        print $fh "echo $lineno{$function}\n";
        print $fh "p \$rip = " . $rips{$function} . "\n";
        print $fh "whatis $function\n";
    } else {
        print $fh "whatis $function\n";
    }
}
close $fh;
system("gdb --nx --nh --command=tmp.gdb emacs > tmp.gdbout");

sub get_type {
    my ($fh) = @_;
    my $type = "";

    my $line;

    while ($type eq "") {
    if (defined $cache) {
        $line = $cache;
    } else {
        die if eof($fh);
        $line = <$fh>;
        if ($line =~ s/^type = //) {
            $type .= $line;
            last;
        }
    }
    }

    chomp $type;
    $type =~ s/\(/\(\*\) \(/ unless $type =~ /\(\*\)/;
    $type =~ s/\(\*\) */\(\*\) /g;

    return $type;
}

open $fh, "<tmp.gdbout" or die;
for my $function (@functions) {
    #    print ("$function has type " .
    ($types{$function} = get_type($fh));

    #print ($types{$function} . " \"calls\" $function\n");

    $callees{$types{$function}}{$function} = $types{$function} . " > " . $function;
}

close $fh;
open $fh, ">tmp.calls.txt" or die;
for my $caller (sort keys %callees) {
    for my $callee (sort keys %{$callees{$caller}}) {
        print $fh $callees{$caller}{$callee} . "\n";
    }
}

close $fh;

exit 0;

if (0) {
# functions that are probably okay:
$callees{memory_full} = { };
$callees{unbind_to} = { };
$callees{handle_interrupt} = { };
$callees{process_quit_flag} = { };
$callees{Fsignal} = { };
$callees{xsignal} = { };
$callees{xsignal1} = { };
$callees{emacs_abort} = { };
$callees{wrong_type_argument} = { };

#check these:
$callees{call0} = { };
$callees{call1} = { };
$callees{call2} = { };
$callees{call3} = { };
$callees{call4} = { };
$callees{call5} = { };
$callees{Fautoload_do_load} = { };
$callees{Fload} = { };
$callees{terminate_due_to_signal} = { };

#indirect calls to check
delete $callees{x_kill_gs_process}{"*"};
delete $callees{hash_lookup}{"*"};
delete $callees{show_mouse_face}{"*"};
delete $callees{decode_coding}{"*"};
delete $callees{detect_coding}{"*"};
delete $callees{define_frame_cursor1}{"*"};
delete $callees{display_and_set_cursor}{"*"};
delete $callees{foreach_window_1}{"*"};
delete $callees{x_build_heuristic_mask}{"*"};
delete $callees{XTtoggle_invisible_pointer}{"*"};
delete $callees{x_to_xcolors}{"*"};
delete $callees{x_draw_right_divider}{"*"};
delete $callees{x_from_xcolors}{"*"};
delete $callees{x_draw_bottom_divider}{"*"};
delete $callees{x_draw_vertical_border}{"*"};
delete $callees{x_destroy_x_image}{"*"};
delete $callees{draw_glyphs}{"*"};
delete $callees{erase_phys_cursor}{"*"};
delete $callees{four_corners_best}{"*"};
delete $callees{image_unget_x_image}{"*"};
delete $callees{internal_condition_case_n}{"*"}; # definitely unsafe
delete $callees{run_timers}{"*"};
delete $callees{gobble_input}{"*"};
delete $callees{hash_remove_from_table}{"*"};
delete $callees{flush_frame}{"*"};
delete $callees{wait_reading_process_output}{"*"}; # definitely unsafe
delete $callees{compute_overhangs_and_x}{"*"};
delete $callees{run_hook_with_args}{"*"}; # unsafe in general
delete $callees{redisplay_internal}{"*"}; # XXX
delete $callees{Ffont_get}{"*"};
delete $callees{Fdelete_terminal}{"*"};
delete $callees{internal_condition_case_1}{"*"};
delete $callees{frame_make_pointer_visible}{"*"};
delete $callees{display_line}{"*"};
delete $callees{draw_fringe_bitmap_1}{"*"};
delete $callees{get_glyph_face_and_encoding}{"*"};
delete $callees{font_done_for_face}{"*"};
delete $callees{get_char_face_and_encoding}{"*"};
delete $callees{update_end}{"*"}; # XXX
delete $callees{font_has_char}{"*"};
delete $callees{update_begin}{"*"};
delete $callees{x_set_fullscreen}{"*"};
delete $callees{get_per_char_metric}{"*"};
delete $callees{font_prop_validate}{"*"};
delete $callees{get_char_glyph_code}{"*"};
delete $callees{traverse_intervals}{"*"}; # XXX
delete $callees{Fraise_frame}{"*"};
delete $callees{clear_font_cache}{"*"};
delete $callees{delete_frame}{"*"}; # unsafe
delete $callees{handle_stop}{"*"};
delete $callees{produce_special_glyphs}{"*"};
delete $callees{move_it_in_display_line_to}{"*"};
delete $callees{append_space_for_newline}{"*"};
delete $callees{get_next_display_element}{"*"};
delete $callees{extend_face_to_end_of_line}{"*"};
delete $callees{cursor_to}{"*"};
delete $callees{get_overlay_arrow_glyph_row}{"*"};
delete $callees{font_matching_entity}{"*"};
delete $callees{traverse_intervals_noorder}{"*"};
delete $callees{font_parse_fcname}{"*"};
delete $callees{update_window}{"*"};
delete $callees{clear_frame}{"*"};
delete $callees{free_image}{"*"};
delete $callees{font_clear_cache}{"*"};
delete $callees{produce_glyphless_glyph}{"*"};
delete $callees{font_update_drivers}{"*"};
delete $callees{font_get_cache}{"*"};
delete $callees{font_open_entity}{"*"};
delete $callees{with_echo_area_buffer}{"*"};# XXX
delete $callees{font_list_entities}{"*"};
delete $callees{line_bottom_y}{"*"};
delete $callees{map_char_table}{"*"};
delete $callees{font_prepare_for_face}{"*"};
delete $callees{write_glyphs}{"*"};
delete $callees{delete_glyphs}{"*"};
delete $callees{insert_glyphs}{"*"};
delete $callees{lookup_image}{"*"};
delete $callees{scrolling_window}{"*"};
delete $callees{clear_end_of_line}{"*"};
delete $callees{update_window_line}{"*"};
delete $callees{redraw_overlapped_rows}{"*"};
delete $callees{redraw_overlapping_rows}{"*"};
delete $callees{font_finish_cache}{"*"};
delete $callees{Fredirect_frame_focus}{"*"};
delete $callees{font_match_p}{"*"};
delete $callees{font_prepare_cache}{"*"};
delete $callees{display_string}{"*"};
delete $callees{map_sub_char_table}{"*"};
delete $callees{ins_del_lines}{"*"};
delete $callees{update_text_area}{"*"};
delete $callees{reset_sys_modes}{"*"};
delete $callees{set_terminal_window}{"*"};
delete $callees{update_marginal_area}{"*"};
delete $callees{internal_condition_case}{"*"};
delete $callees{font_encode_char}{"*"};
delete $callees{valid_image_p}{"*"};
delete $callees{map_keymap_item}{"*"};
delete $callees{init_sys_modes}{"*"};
delete $callees{define_image_type}{"*"};
delete $callees{prepare_image_for_display}{"*"};
delete $callees{composition_gstring_put_cache}{"*"};
delete $callees{x_create_bitmap_mask}{"*"};
delete $callees{map_charset_chars}{"*"};
delete $callees{map_charset_for_dump}{"*"};
delete $callees{map_char_table_for_charset}{"*"};
delete $callees{x_set_frame_parameters}{"*"};
delete $callees{font_fill_lglyph_metrics}{"*"};
delete $callees{map_sub_char_table_for_charset}{"*"};
delete $callees{xg_get_pixbuf_from_pixmap}{"*"};

#these are not okay, but happen anyway:
$callees{Fmessage} = { };
$callees{message1} = { };
$callees{message3} = { };
$callees{message3_nolog} = { };

$callees{encode_coding_object} = { };
}

my %path;

sub sortab {
    my ($a, $b) = @_;
    my ($ag, $bg);
    my ($an, $bn);

    $ag = $a;
    $ag = s/[^>]//g;
    $bg = $b;
    $bg = s/[^>]//g;

    $an = $a;
    $an =~ s/.* > //g;
    $bn = $b;
    $bn =~ s/.* > //g;

    return (length($ag) <=> length($bg) ||
            $an cmp $bn);
}

#for my $caller (sort keys %callees) {
for my $caller ("delete_terminal") {
    %path = %callees;
    my $didsomething = 1;

    while ($didsomething) {
        warn "iteration " . (++$iterations);
        $didsomething = 0;

        for my $intermediate (keys %{$path{$caller}}) {
            next if $intermediate eq $caller;
            for my $final (keys %{$callees{$intermediate}}) {
                next if $final eq $caller or $final eq $intermediate;
                my $newpath = $path{$caller}{$intermediate} . " > $final";
                my $oldpath = $path{$caller}{$final};
                if ($oldpath eq "") {
                    $didsomething = 1;
                }
                if ($oldpath eq "" or
                    length($newpath) < length($oldpath)) {
                    $path{$caller}{$final} = $newpath;
                }
            }
        }
    }

    for my $callee (sort { sortab($a, $b) } keys %{$path{$caller}}) {
        print $path{$caller}{$callee} . "\n";
    }
}
