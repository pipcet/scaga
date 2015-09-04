use IPC::Run qw(run new_chunker);
use File::Slurp qw(read_file);
use Getopt::Long;
use strict;
use Scaga;

my $strip_identifier = 0;
my $strip_codeline = 1;
my $strip_flc = 1;
my $strip_value = 1;
my $strip_component = 0;
my $strip_intype = 0;
my $do_strip_cycles = 1;

GetOptions("strip_identifier=i" => \$strip_identifier,
           "strip_codeline=i" => \$strip_codeline,
           "strip_flc=i" => \$strip_codeline,
           "strip_value=i" => \$strip_value,
           "strip_component=i" => \$strip_component,
           "strip_intype=i" => \$strip_intype,
           "strip_cycles=i" => \$do_strip_cycles);

while (<>) {
    chomp;
    my $path = Scaga::Path->new($_);

    if ($do_strip_cycles and $path->cycle) {
        next;
    }

    for my $ppath (@{$path->{ppaths}}) {
        for my $pattern (@{$ppath->{patterns}}) {
            my @comps;
            for my $component (@{$pattern->{components}}) {
                push @comps, $component if $component->isa('Scaga::Component::FLC') && !$strip_flc;
                push @comps, $component if $component->isa('Scaga::Component::Codeline') && !$strip_codeline;
                push @comps, $component if $component->isa('Scaga::Component::Identifier') && !$strip_identifier;
                push @comps, $component if $component->isa('Scaga::Component::Value') && !$strip_value;
                push @comps, $component if $component->isa('Scaga::Component::Component') && !$strip_component;
            }
            $pattern->{components} = \@comps;
        }
    }
    print $path->repr . "\n";
}
