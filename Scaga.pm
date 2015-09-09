use Carp::Always;
use strict;

package Scaga::Component;
use Data::Dumper;

sub identifiers {
    return ();
}

sub repr {
    my ($self) = @_;

    die;
}

sub match {
    my ($self, $other, $param) = @_;

    if (defined($self->{pattern}) and defined($other->{pattern})) {
        return $self->{pattern} eq $other->{pattern};
    }

    die if (defined($self->{pattern}));

    if (defined($self->{codeline}) && defined($other->{pattern})) {
        my $pattern = $other->{pattern};
        my $m = $self->{codeline} =~ /$pattern/;

        return $m;
    }

    for my $lkey (keys %$self) {
        next if $lkey eq "codeline";
        next if $lkey eq "flc";
        next if $lkey eq "home";

        if (defined($other->{$lkey}) and
            $other->{$lkey} ne $self->{$lkey}) {
            return 0;
        }

        if ($param && $param->{lstrict}->{$lkey} &&
            !defined($other->{$lkey})) {
            return 0;
        }
    }

    return 1;
}

sub new {
    my ($class, $string, $output) = @_;

    if ($string =~ /^FLC:(.*?)$/) {
        return Scaga::Component::FLC->new($1);
    } elsif ($string =~ /^home:(.*?)$/) {
        return Scaga::Component::Home->new($1);
    } elsif ($string =~ /^(\/[^\/]*\/)$/) {
        return Scaga::Component::RegExp->new($1);
    } elsif ($string =~ /^\'([^']*)\'$/) {
        return Scaga::Component::Codeline->new($1);
    } elsif ($string =~ /0x/) {
        return Scaga::Component::Value->new($string);
    } else {
        return Scaga::Component::Identifier->new($string);
    }
}

package Scaga::Component::RegExp;
use parent -norequire, 'Scaga::Component';
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $value ($self->{pattern}) {
        $ret .= HTML::Entities::encode_entities($value);
    }

    return $ret;
}


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
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $value ($self->{identifier}) {
        $ret .= HTML::Entities::encode_entities($value);
    }

    return $ret;
}


sub identifiers {
    my ($self) = @_;

    return ($self->{identifier});
}

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
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $value ($self->{codeline}) {
        $ret .= HTML::Entities::encode_entities($value);
    }

    return $ret;
}


sub repr {
    my ($self) = @_;

    return "'" . $self->{codeline} . "'";
}

sub new {
    my ($class, $string, $output) = @_;

    $string =~ s/\'//msg;

    return bless { codeline => $string }, $class;
}

package Scaga::Component::FLC;
use parent -norequire, 'Scaga::Component';
use HTML::Entities;
use Data::Dumper;

sub grab_context {
    my ($file, $line, $column) = @_;

    my $fh;
    open $fh, "</home/pip/git/emacs/src/$file" or return "<pre>cannot open $file</pre>";

    my @lines = <$fh>;

    return "<pre>" .
        HTML::Entities::encode_entities(join("", @lines[$line-21..$line-2])) .
        "<a href=\"file:///home/pip/git/emacs/src/$file:$line\">" .
        HTML::Entities::encode_entities($lines[$line-1]) .
        "</a>" .
        HTML::Entities::encode_entities(join("", @lines[$line..$line+19])) . "</pre>\n";
}

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $value ($self->{flc}) {
        #        $ret .= HTML::Entities::encode_entities($value);
        $self->{flc} =~ /^(.*?):(.*?):(.*?)$/;
        $ret .= grab_context($1, $2, $3);
    }

    return $ret;
}


sub repr {
    my ($self) = @_;

    return "FLC:" . $self->{flc};
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { flc => $string }, $class;
}

package Scaga::Component::Home;
use parent -norequire, 'Scaga::Component';
use HTML::Entities;
use Data::Dumper;

sub grab_context {
    my ($file, $line, $column) = @_;

    my $fh;
    open $fh, "</home/pip/git/emacs/src/$file" or return "<pre>cannot open $file</pre>";

    my @lines = <$fh>;

    return "<pre>" .
        HTML::Entities::encode_entities(join("", @lines[$line-21..$line-2])) .
        "<a href=\"file:///home/pip/git/emacs/src/$file:$line\">" .
        HTML::Entities::encode_entities($lines[$line-1]) .
        "</a>" .
        HTML::Entities::encode_entities(join("", @lines[$line..$line+19])) . "</pre>\n";
}

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $value ($self->{home}) {
        #        $ret .= HTML::Entities::encode_entities($value);
        $self->{home} =~ /^(.*?):(.*?):(.*?)$/;
        $ret .= grab_context($1, $2, $3);
    }

    return $ret;
}


sub repr {
    my ($self) = @_;

    return "home:" . $self->{home};
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { home => $string }, $class;
}

package Scaga::Component::Value;
use parent -norequire, 'Scaga::Component';
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $value ($self->{value}) {
        $ret .= HTML::Entities::encode_entities($value);
    }

    return $ret;
}


sub repr {
    my ($self) = @_;

    return $self->{value};
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { value => $string }, $class;
}

package Scaga::Component::Intype;
use parent -norequire, 'Scaga::Component';
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $value ($self->{intype}) {
        $ret .= HTML::Entities::encode_entities($value);
    }

    return $ret;
}


sub repr {
    my ($self) = @_;

    return $self->{intype};
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { intype => $string }, $class;
}

package Scaga::Component::Component;
use parent -norequire, 'Scaga::Component';
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $value ($self->{component}) {
        $ret .= HTML::Entities::encode_entities($value);
    }

    return $ret;
}

sub repr {
    my ($self) = @_;

    return $self->{component};
}

sub new {
    my ($class, $string, $output) = @_;

    return bless { component => $string }, $class;
}

package Scaga::Pattern;
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;

    my $ret = "";

    for my $component (@{$self->{components}}) {
        $ret .= "<p>\n";
        $ret .= $component->html;
        $ret .= "</p>\n";
    }

    return $ret;
}

# internal. modifies data.
sub normalize {
    my ($self) = @_;

#    die $self->repr unless $self->match($self);

    my $components = $self->{components};

  loop:
    while (1) {
        for my $i (0 .. $#$components) {
            for my $j ($i + 1 .. $#$components) {
                if (ref($components->[$i]) eq ref($components->[$j])) {
                    splice @$components, $j, 1;
                    next loop;
                }
            }
        }
        last;
    }
}

sub identifier {
    my ($self) = @_;

    for my $component (@{$self->{components}}) {
        if ($component->isa('Scaga::Component::Identifier')) {
            return $component->{identifier};
        }
    }

    return undef;
}

sub repr {
    my ($self) = @_;

    return join(" = ", map { $_->repr } @{$self->{components}});
}

sub identifiers {
    my ($self) = @_;
    my @ret;

    for my $ppath (@{$self->{components}}) {
        push @ret, $ppath->identifiers;
    }

    return @ret;
}

use Data::Dumper;

sub match {
    my ($self, $other, $param) = @_;

    die $self->repr unless @{$self->{components}} and @{$other->{components}};

    for my $lcomp (@{$self->{components}}) {
        for my $rcomp (@{$other->{components}}) {
            if (!$lcomp->match($rcomp, $param)) {
                return 0;
            }
        }
    }

    return 1;
}

sub merge {
    my ($self, $other) = @_;

    die ($self->repr . " doesn't match " . $other->repr) unless $self->match($other);

    my @components;
    push @components, @{$self->{components}};
    push @components, @{$other->{components}};

    my $ret = bless { components => \@components }, 'Scaga::Pattern';

    $ret->normalize;

    return $ret;
}

sub new {
    my ($class, $string, $output) = @_;
    my @components;

    while ($string) {
        if ($string =~ s/^FLC:(.*?:[0-9]*:[0-9]*)//ms) {
            push @components, Scaga::Component::FLC->new($1);
        } elsif ($string =~ s/^home:(.*?:[0-9]*:[0-9]*)//ms) {
            push @components, Scaga::Component::Home->new($1);
        } elsif ($string =~ s/^(component:[^ =]*)//ms) {
            push @components, Scaga::Component::Component->new($1);
        } elsif ($string =~ s/^(intype:[^=]*[^ =])//ms) {
            push @components, Scaga::Component::Intype->new($1);
        } elsif ($string =~ s/^(\/[^\/]*\/)//ms) {
            push @components, Scaga::Component::RegExp->new($1);
        } elsif ($string =~ s/^'([^']*)'//ms) {
            push @components, Scaga::Component::Codeline->new($1);
        } elsif ($string =~ /^'/) {
            die $string;
        } elsif ($string =~ s/^([^=]*0x[^=]*[^ =])//ms) {
            push @components, Scaga::Component::Value->new($1);
        } elsif ($string =~ s/^([^=]*[^ =])//ms) {
            push @components, Scaga::Component::Identifier->new($1);
        }

        die $string unless $string eq "" or $string =~ s/^ = //ms;
    }

    my $ret = bless { components => \@components }, $class;

    $ret->normalize;

    return $ret;
}

package Scaga::PPath;

sub repr {
    my ($self) = @_;

    return join(" > ", map { $_->repr } @{$self->{patterns}});
}

sub identifiers {
    my ($self) = @_;
    my @ret;

    for my $ppath (@{$self->{patterns}}) {
        push @ret, $ppath->identifiers;
    }

    return @ret;
}

sub n {
    my ($self) = @_;

    return scalar(@{$self->{patterns}});
}

sub concat_overlapping {
    my ($self, $other, $overlap) = @_;

    my @patterns = @{$self->{patterns}};
    my $n0 = scalar @patterns;
    push @patterns, @{$other->{patterns}};

    splice @patterns, $n0 - 1, 2, $patterns[$n0-1]->merge($patterns[$n0]);

    return bless { patterns => \@patterns }, 'Scaga::PPath';
    my $ppath = $self->{ppaths}->[0]->concat_overlapping($other->{ppaths}->[0], $overlap);

    return bless { ppaths => [$ppath] }, 'Scaga::Path';
}

sub concat {
    my ($self, $other) = @_;

    my @patterns = @{$self->{patterns}};
    push @patterns, @{$other->{patterns}};

    return bless { patterns => \@patterns }, 'Scaga::PPath';
}

sub slice {
    my ($self, $i, $j) = @_;

    if (!defined($i) or $i < 0) {
        $i = 0;
    }
    if (!defined($j) or $j > $self->n) {
        $j = $self->n;
    }

    my @spatterns = @{$self->{patterns}}[$i .. $j-1];

    return bless { patterns => \@spatterns }, 'Scaga::PPath';
}

sub match {
    my ($self, $other, $param) = @_;

    die unless $other->isa('Scaga::PPath');

    my $ln = $self->n;
    my $rn = $other->n;

    if ($ln == $rn) {
        my $n = $ln;

        for my $i (0..$n-1) {
            if (!$self->{patterns}->[$i]->match($other->{patterns}->[$i], $param)) {
                return undef;
            }
        }

        return [0, $n];
    }

    return undef;
}

sub new {
    my ($class, $string, $output) = @_;

    my @patterns = split(" > ", $string);

    @patterns = map { Scaga::Pattern->new($_) } @patterns;

    return bless { patterns => \@patterns }, $class;
}

package Scaga::Path;
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;
    my $n = $self->n;

    my $ret = "";

    $ret .= "<table>\n";
    $ret .= "<tr style=\"width: 30em\">\n";
    $ret .= "<td colspan=\"$n\">\n";
    $ret .= "<pre>" . HTML::Entities::encode_entities($self->repr) . "</pre>\n";
    $ret .= "</td>\n";
    $ret .= "</tr>\n";
    $ret .= "<tr>\n";

    for my $i (0 .. $n-1) {
        $ret .= "<td>\n";
        $ret .= $self->slice($i, $i+1)->{ppaths}->[0]->{patterns}->[0]->html;
        $ret .= "</td>\n";
    }

    $ret .= "</tr></table>\n";

    return $ret;
}


%Scaga::Path::paths = ();

sub intern {
    my ($self) = @_;

    return $self if $self->{interned};

    my $repr = $self->repr;
    return $Scaga::Path::paths{$repr} if $Scaga::Path::paths{$repr};

    $self->{interned} = 1;
    #$Scaga::Path::paths{$repr} = $self;

    return $self;
}

sub repr {
    my ($self) = @_;

    return $self->{repr} if (defined $self->{repr});

    return $self->{repr} = join(" >> ", map { $_->repr } @{$self->{ppaths}});
}

sub short_repr {
    my ($self, $last, $keep) = @_;
    my $origlast = $last;
    return $self->{short_repr}->{$last} if exists $self->{short_repr}->{$last};

    my $spath = $self->slice($self->n - $last, $self->n);
    while (scalar grep { !$keep->{$_} } $spath->identifiers < $origlast) {
        $last++;
        $spath = $self->slice($self->n - $last, $self->n);
        last if $last >= $self->n;
    }

    my $repr = $spath->repr;

    $self->{short_repr}->{$origlast} = $repr;

    return $repr;
}

sub identifiers {
    my ($self) = @_;

    if (defined($self->{identifiers})) {
        return @{$self->{identifiers}};
    }

    my @ret;

    for my $ppath (@{$self->{ppaths}}) {
        push @ret, $ppath->identifiers;
    }

    $self->{identifiers} = \@ret;

    return @ret;
}

sub last_identifier {
    my ($self) = @_;

    my @identifiers = $self->identifiers;

    return $identifiers[$#identifiers];
}

sub n {
    my ($self) = @_;
    my $n = 0;

    for my $ppath (@{$self->{ppaths}}) {
        $n += $ppath->n;
    }

    return $n;
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
    my ($self, $other, $param) = @_;

    die unless $other->isa('Scaga::Path');

    die("LHS must be a PPath, but " . $self->repr . " isn't one.") unless @{$self->{ppaths}} == 1;

    my $n = scalar(@{$self->{ppaths}->[0]->{patterns}});

    for my $i (0 .. $n) {
        for my $j ($i+1 .. $n) {
            my $ppath = $self->{ppaths}->[0]->slice($i, $j);

            my $subpath = bless { ppaths => [$ppath] }, 'Scaga::Path';

            if ($subpath->match($other, $param)) {
                return [$i, $j];
            }
        }
    }

    return undef;
}

sub slice {
    my ($self, $i, $j) = @_;

    die("LHS must be a PPath, but " . $self->repr . " isn't one.") unless @{$self->{ppaths}} == 1;

    my $ppath = $self->{ppaths}->[0]->slice($i, $j);

    return bless { ppaths => [$ppath] }, 'Scaga::Path';
}

sub concat_overlapping {
    my ($self, $other, $overlap) = @_;

    die("LHS must be a PPath, but " . $self->repr . " isn't one.") unless @{$self->{ppaths}} == 1;
    die "RHS must be a PPath" unless @{$other->{ppaths}} == 1;

    my $ppath = $self->{ppaths}->[0]->concat_overlapping($other->{ppaths}->[0], $overlap);

    return bless { ppaths => [$ppath] }, 'Scaga::Path';
}

sub concat {
    my ($self, $other) = @_;

    die("LHS must be a PPath, but " . $self->repr . " isn't one.") unless @{$self->{ppaths}} == 1;
    die "RHS must be a PPath" unless @{$other->{ppaths}} == 1;

    my $ppath = $self->{ppaths}->[0]->concat($other->{ppaths}->[0]);

    return bless { ppaths => [$ppath] }, 'Scaga::Path';
}

use Data::Dumper;

sub endmatch {
    my ($self, $other, $param) = @_;

    die unless $other->isa('Scaga::Path');

    die("LHS must be a PPath, but " . $self->repr . " isn't one.") unless @{$self->{ppaths}} == 1;

    my $n = $self->n;

    for my $i (reverse (0 .. $n-1)) {
        my $subpath = $self->slice($i, $n);

        if ($subpath->match($other, $param)) {
            return [$i, $n];
        }
    }

    return undef;
}

sub match {
    my ($self, $other, $param) = @_;

    die unless $other->isa('Scaga::Path');

    die("LHS must be a PPath, but " . $self->repr . " isn't one.") unless @{$self->{ppaths}} == 1;

    if (@{$other->{ppaths}} == 1) {
        my $m = $self->{ppaths}->[0]->match($other->{ppaths}->[0], $param);

        return $m if $m;
    }

    if (@{$other->{ppaths}} == 2) {
        my $n = @{$self->{ppaths}->[0]->{patterns}};
        for my $i (0 .. @{$self->{ppaths}->[0]->{patterns}}) {
            for my $j ($i + 1 .. $n) {
                my $lslice = $self->{ppaths}->[0]->slice(0, $i);
                my $rslice = $self->{ppaths}->[0]->slice($j, $n);

                if ($lslice->match($other->{ppaths}->[0], $param) &&
                    $rslice->match($other->{ppaths}->[1], $param)) {
                    return [$i, $j];
                }
            }
        }
    }

    return undef;
}

sub cycle {
    my ($self) = @_;
    if (exists $self->{cycle}) {
        return $self->{cycle};
    }
    my @identifiers = $self->identifiers;
    for my $i (0 .. $#identifiers) {
        for my $j ($i+1 .. $#identifiers) {
            if ($identifiers[$i] eq $identifiers[$j]) {
                return $self->{cycle} = [$i, $j];
            }
        }
    }

    return $self->{cycle} = undef;
}

sub cmp {
    my ($self, $other) = @_;

    return 1024 * ($self->n <=> $other->n) || ($self->repr cmp $other->repr);
}

package Scaga::Rule;
use HTML::Entities;
use Data::Dumper;

sub html {
    my ($self) = @_;

    my $ret = "";

    $ret .= "<table>\n";



    $ret .= "</table>\n";

    return $ret;
}

sub identifiers {
    my ($self) = @_;

    return $self->{in}->identifiers;
}

use Data::Dumper;
use HTML::Entities;

sub substitute {
    my ($self, $input, $param) = @_;
    my @res = ();

    my $m;
    if ($m = $input->submatch($self->{in}, $param)) {
        if ($self->{file} eq "emacs-rules-tofix.scaga") {
        my $fh;
        open $fh, ">>match.html";
        print $fh "<pre>" . HTML::Entities::encode_entities($self->{file} . ":" . $self->{line}) . "</pre>\n";
        print $fh "<pre>" . HTML::Entities::encode_entities($self->{in}->repr) . "</pre>\n";
        print $fh "<pre>" . HTML::Entities::encode_entities($input->repr) . "</pre>\n";
        print $fh $input->slice($m->[0], $m->[1])->html;
        close $fh;
        }
        $self->{usecount}++;
        if ($self->{out}) {
            my $output = $input->slice(0, $m->[0])
                ->concat($self->{out})
                ->concat($input->slice($m->[1], $input->n));

            push @res, $output;
        } else {
            return [];
        }
    }

    if (@res) {
        return \@res;
    } else {
        return undef;
    }
}

sub repr {
    my ($self) = @_;

    if ($self->{out}) {
        return $self->{kind} . " := " . $self->{in}->repr . " => " . $self->{out}->repr;
    } else {
        return $self->{kind} . " := " . $self->{in}->repr;
    }
}

sub new {
    my ($class, $string) = @_;
    my $self = bless { }, $class;

    if ($string =~ s/^(.*?) := //) {
        my $kind = $1;

        $self->{kind} = $kind;
    } else {
        $self->{kind} = "drop";
    }

    if ($string =~ /^(.*) => (.*)$/) {
        my ($in, $out) = ($1, $2);

        $self->{in} = Scaga::Path->new($in);
        $self->{out} = Scaga::Path->new($out);
        $self->{usecount} = 0;

        return $self;
    }

    if ($string =~ /^(.*?)( =>)?$/) {
        my ($in, $out) = ($1, '');

        $self->{in} = Scaga::Path->new($in);
        $self->{usecount} = 0;

        return $self;
    }

    die "$string";
    return undef;
}

package Scaga;

sub cmppath {
    my ($self, $other) = @_;

    return $self->cmp($other);
}

1;
