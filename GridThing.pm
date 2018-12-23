package GridThing;

use strict;

use base qw(Class::Accessor::Fast);

use OpenGL qw(:glconstants :glfunctions :glufunctions);
use Wx qw(wxTheApp);
use Math::VectorReal;
#use Graphics::ColorObject;
use Data::Dumper qw(Dumper);
use List::MoreUtils qw(firstidx mesh);
use Storable qw(dclone);
use Math::Trig;
use Log::Log4perl qw(get_logger);
use YAML qw(LoadFile);
use Math::Geometry::Planar;

__PACKAGE__->mk_accessors( qw(name type state offset_v color_v selected_color_v dim_v id rotatex rotatey rotatez texture texture_infotile texture_infofactor
    grid_offset_v grid_cell_size grid_x_count grid_z_count sides wall axis angle direction steps smooth ratiotop ratiobottom
    pattern
    gridcountx gridspacex gridcounty gridspacey gridcountz gridspacez edgex edgey edgez
    ringradius ringcount ringstart ringangle ringstretchx ringstretchy centred_on centred_position orient climb
    swapx swapy swaptexture
    legs base square
    shape stretch
    needs_compile
    pattern_max pattern_min current_shift
    ) );

# variables {{{1

my $log = get_logger;

# COPIED FROM grid.pl
my $TRANSPARENT_ALPHA_05 = 0.5;
my $TRANSPARENT_ALPHA_03 = 0.3;

# COPIED FROM grid.pl
my $axis_dim = {
    0 => {
        x => 2,
        y => 1,
    },
    1 => {
        x => 0,
        y => 2,
    },
    2 => {
        x => 0,
        y => 1,
    },
};

# thing state flags
our (
    $TS_SELECTED,
    $TS_SELECTED_FROM_LIST,
    $TS_TRANSPARENT,
    $TS_HIDDEN,
    $TS_WIREFRAME,
    $TS_SHOW_VERTICES,
    $TS_NO_RENDER,
    $TS_DEBUG,
    $TS_ADJUSTING,
) = map { 2 ** $_ } (0 .. 15);

# thing types
our (
    $TT_CUBOID,
    $TT_BOX,
    $TT_PRISM,
    $TT_STAIRS,
    $TT_ARCH,
    $TT_SPHERE,
    $TT_SHAPE,
    ) = our @thing_types = qw(Cuboid Box Prism Stairs Arch Sphere Shape);

# thing patterns
our (
    $TP_SINGLE,
    $TP_GRID,
    $TP_RING,
    ) = our @patterns = qw(Single Grid Ring);;

# default colors; used to init the color pickers as well as supplying
# default values in the constructor.
our $COLOR = [ 1, 1, 1, ];
#our $COLOR = [ 0.32, 0.76, 0.80, ];
our $SELECTED_COLOR = [ 0.8, 0.8, 0 ];

my @other_dims = (
    [ 1, 2 ],
    [ 0, 2 ],
    [ 0, 1 ],
);

# functions {{{1

#*******************************************************************************
sub TYPE_SETTINGS { # {{{2
    my ($class, $type) = @_;

    my @standard = qw(name color_v selected_color_v rotate texture texture_info);

    my $extra_type_settings = {
        $TT_CUBOID => [ ],
        $TT_BOX => [ qw(wall) ],
        $TT_PRISM => [ qw(sides smooth axis angle ratio ) ],
        $TT_STAIRS => [ qw(steps direction axis) ],
        $TT_ARCH => [ qw(legs base direction steps square ) ],
        $TT_SPHERE => [ qw(steps smooth) ],
        $TT_SHAPE => [ qw(shape stretch axis swap) ],
    };

    # build a temporary hash with the settings we want
    my %setting_shown;
    map { $setting_shown{$_} = 1 } (@standard, @{ $extra_type_settings->{$type} });

    return \%setting_shown;
}
    
#*******************************************************************************
sub PATTERN_SETTINGS { # {{{2
    my ($class, $pattern) = @_;

    $log->logconfess("no pattern") unless defined $pattern;

    my $pattern_settings = {
        $TP_SINGLE => [ ],
        $TP_GRID => [ qw(gridcount gridspace edge) ],
        $TP_RING => [ qw(ring centred_on centred_position orient climb) ],
    };

    # build a temporary hash with the settings we want
    my %setting_shown;
    map { $setting_shown{$_} = 1 } ( @{ $pattern_settings->{$pattern} });

    return \%setting_shown;
}
    
#*******************************************************************************
sub new {           # {{{2
    my( $class, $arg ) = @_;
    my $self = $class->SUPER::new();

    for my $key (keys %{ $arg }) {
        $self->{$key} = $arg->{$key};
    }

    my $app = wxTheApp;

    # defaults
    if ($self->{name} eq $self->{type}) {
        $self->{name} .= scalar @{ $app->things };
    }
    $self->{color_v} ||= [ @{ $COLOR } ];
    $self->{selected_color_v} ||= [ @{ $SELECTED_COLOR } ];
    $self->{pattern} ||= $TP_SINGLE;
    $self->{state} ||= 0;

    my $build_grid = $app->build_grid;

    # record grid settings at construction
    $self->grid_offset_v( [ @{ $build_grid->{offset_v} } ]);
    $self->grid_x_count( $build_grid->{major_count} );
    $self->grid_z_count( $build_grid->{minor_count} );
    $self->grid_cell_size( $build_grid->{cell_size} );

    # initial render calculations
    $self->calculate_for_render;

    bless ($self, $class);
    return $self;
}

#*******************************************************************************
sub do_delete { #{{{2
    my ($self) = @_;
    $log->debug("do_delete");

    my $app = wxTheApp;

    # do standard unselection stuff
    $self->clear_state($TS_SELECTED) if $self->state & $TS_SELECTED;

    # remove from app list
    $log->debug("splice out " . $self->id . " from " . join(',', map { $_->id } @{ $app->things }));
    splice @{ $app->things }, $self->id, 1;
    $log->debug("spliced");

    # remove from properties window list
    $app->{prop_frame}->remove_thing($self);

    # adjust ids on things above the deleted thing in the app list, also
    # recompile so the display list id is also adjusted.
    for my $id ($self->id .. $#{ $app->things }) {
        $log->debug("adjust id $id");
        $app->things->[$id]->id($id);
        $app->things->[$id]->needs_compile(1);
    }

    return;
}

#*******************************************************************************
#sub DESTROY { #{{{2
#    my ($self) = @_;
#    print STDERR "thing dying: ", $self->name, "\n";
#
#    # do standard unselection stuff
#    $self->clear_state($TS_SELECTED) if $self->state & $TS_SELECTED;
#
#    return;
#}
#
#*******************************************************************************
sub set_state { #{{{2
    my ($self, $state_flags) = @_;

    if ($self->state & $TS_DEBUG) {
        $log->debug(sprintf "set_state %04X", $state_flags);
    }
#    else {
#        $log->debug("set_state $state_flags");
#    }

    # TS_SELECTED_FROM_LIST is a pseudo-flag used to distinguish canvas selection from list selection
    my $selected_from_list = 0;
    if ($state_flags & $TS_SELECTED_FROM_LIST) {
        $state_flags &= ~$TS_SELECTED_FROM_LIST;
        $state_flags |= $TS_SELECTED;
        $selected_from_list = 1;
    }

    $self->state($self->state | $state_flags);

    # common selection actions
    if ($state_flags & $TS_SELECTED) {
        my $app = wxTheApp;
        unshift @{ $app->selected_things }, $self;
        $log->debug("select thing " . $self->id);

        if (scalar  @{ $app->selected_things } == 1) {

            # restore grid to what it was at our construction
            my $build_grid = $app->build_grid;
            $build_grid->{offset_v}->[0] = $self->grid_offset_v->[0];
            $build_grid->{offset_v}->[1] = $self->grid_offset_v->[1];
            $build_grid->{offset_v}->[2] = $self->grid_offset_v->[2];
            $build_grid->{major_count} = $self->grid_x_count;
            $build_grid->{minor_count} = $self->grid_z_count;
            $build_grid->{cell_size} = $self->grid_cell_size;
        }

        $self->mark_on_grid;
        $app->{prop_frame}->select_thing($self) unless $selected_from_list;
    }

    $self->needs_compile(1);

    return $self->state;
}

#*******************************************************************************

sub clear_state { #{{{2
    my ($self, $state_flags) = @_;

    my $app = wxTheApp;

    if ($self->state & $TS_DEBUG) {
        $log->debug(sprintf "clear_state %04X", $state_flags);
    }
#    else {
#        $log->debug("clear_state $state_flags");
#    }

    # TS_SELECTED_FROM_LIST is a pseudo-flag used to distinguish canvas selection from list selection
    my $selected_from_list = 0;
    if ($state_flags & $TS_SELECTED_FROM_LIST) {
        $state_flags &= ~$TS_SELECTED_FROM_LIST;
        $state_flags |= $TS_SELECTED;
        $selected_from_list = 1;
    }

    # common de-selection actions which require the flag on
    if ($state_flags & $TS_SELECTED) {
    }

#    $log->debug("clear_state $state_flags");
    $self->state($self->state & ~$state_flags);

    # common de-selection actions which require the flag off
    if ($state_flags & $TS_SELECTED) {

        $log->debug("deselect thing " . $self->id . " from " . join(',', @{ $app->selected_things }));
        if (defined(my $index = firstidx { $_ == $self } @{ $app->selected_things })) {
            splice @{ $app->selected_things }, $index, 1;
        }
        else {
            $log->logdie("couldn't find deselected thing in selected_things");
        }

        $self->mark_on_grid;
        $app->{prop_frame}->select_thing($self, 0) unless $selected_from_list;
    }

    $self->needs_compile(1);

    return $self->state;
}

#*******************************************************************************

sub mark_on_grid { #{{{2
    my ($self) = @_;

    my $build_grid = wxTheApp->build_grid;

    $log->debug("mark_on_grid: " . sprintf("%.2f, %.2f", $self->grid_cell_size, $build_grid->{cell_size}));

    # don't display on the grid if the cell size doesn't match
#    return if $self->grid_cell_size != $build_grid->{cell_size};

    my $inc = ($self->state() & $TS_SELECTED) ? 1 : -1;

    # find build grid cells that intersect with a slightly smaller version
    # of ourself (avoid floating point edge cases)
    my $min_x = $self->offset_v->[0] + 0.01;
    my $max_x = $self->offset_v->[0] + $self->dim_v->[0] - 0.01;
    my $min_z = $self->offset_v->[2] + 0.01;
    my $max_z = $self->offset_v->[2] + $self->dim_v->[2] - 0.01;

    my $grid_index = 0;
    for my $x_cell ( 0 .. $build_grid->{major_count} - 1 ) {
        my $x_base = $build_grid->{offset_v}->[0] + $x_cell * $build_grid->{cell_size};
        for my $z_cell ( 0 .. $build_grid->{minor_count} - 1 ) {
            my $z_base = $build_grid->{offset_v}->[2] + $z_cell * $build_grid->{cell_size};
            if (($x_base < $max_x && $x_base + $build_grid->{cell_size} > $min_x)
                || ($z_base < $max_z && $z_base + $build_grid->{cell_size} > $min_z))
            {
                $build_grid->{selected_count}->[$grid_index] += $inc;
            }
            $grid_index++;
        }
    }
    $build_grid->{needs_compile} = 1;

    return;
}

#*******************************************************************************

sub toggle_state { #{{{2
    my ($self, $state_flags) = @_;

    $log->debug("toggle_state $state_flags");

    if ($self->state & $state_flags) {
        $self->clear_state($state_flags);
    }
    else {
        $self->set_state($state_flags);
    }

    return $self->state;
}

#*******************************************************************************
# Uses a thing's adjustable settings to calculate internal vertex lists. Separately
# works out list of pattern shifts if a pattern is in use. Nothing goes to OpenGL
# from here, that's done in Render. This routine is only called when an object is
# modified.
sub calculate_for_render { #{{{2
    my ($self) = @_;

    my $dim_v = $self->dim_v;

    # check that we have 3 non-zero dims
    if ($dim_v->[0] && $dim_v->[1] && $dim_v->[2]) {
        $self->clear_state($TS_NO_RENDER) if $self->state & $TS_NO_RENDER;
    }
    else {
        $self->set_state($TS_DEBUG);
        $self->set_state($TS_NO_RENDER) unless $self->state & $TS_NO_RENDER;
        $log->debug(sprintf("not calcing for render due to zero dim %s %08x", $self->name, $self->state));
        return;
    }

    $self->needs_compile(1);

    my $grid_cell_size = $self->grid_cell_size;

    my @pattern_shifts;

    my $app = wxTheApp;

    # patterns {{{3
    if ($self->pattern eq $TP_SINGLE) {
        # the single pattern is handled by default in render()
        @pattern_shifts = ( 0 );
    }
    elsif ($self->pattern eq $TP_GRID) {

        for my $x_index (0 .. $self->{gridcountx} - 1) {
            for my $y_index (0 .. $self->{gridcounty} - 1) {
                for my $z_index (0 .. $self->{gridcountz} - 1) {
                    my $skip = undef;
                    if ($self->{edgex}) {
                        $skip = ( $x_index > 0 && $x_index < $self->{gridcountx} - 1);
                    }
                    if ($self->{edgey}) {
                        $skip ||= 0;
                        $skip &&= ($y_index > 0 && $y_index < $self->{gridcounty} - 1);
                    }
                    if ($self->{edgez}) {
                        $skip ||= 0;
                        $skip &&= ($z_index > 0 && $z_index < $self->{gridcountz} - 1);
                    }
                    next if $skip;
                    push @pattern_shifts,
                        [
                            $x_index * $self->{gridspacex} * $grid_cell_size,
                            $y_index * $self->{gridspacey} * $grid_cell_size,
                            $z_index * $self->{gridspacez} * $grid_cell_size,
                        ];
                }
            }
        }

    }
    elsif ($self->pattern eq $TP_RING) {

        my $max_stretch = List::Util::max($self->ringstretchx, $self->ringstretchy);

        my $start = $self->ringstart;
        my $angle = $self->ringangle || (360 / $self->ringcount);
        if ($self->orient && ! $start) {
            $start = 90;
        }

        my $rotation_centre = [0,0,0];
        my $radius = $self->ringradius * $grid_cell_size;
        if ($self->centred_on) {
            my @things = grep { $_->{name} eq $self->centred_on } @{ $app->things };
            if (scalar @things == 1) {
                my $centre_thing = $things[0];
                $rotation_centre = [ map { ($centre_thing->offset_v->[$_] + $centre_thing->dim_v->[$_] / 2) - ($self->offset_v->[$_] + $self->dim_v->[$_] / 2) } (0 .. 2) ];
                $rotation_centre->[1] = 0;

                # do we adjust the radius?
                if ($self->centred_position > 0) {
                    $log->info("adjust radius to inner/outer edge");

                    $radius = $centre_thing->ringradius * $centre_thing->grid_cell_size + ($self->centred_position == 1 ? -1 : 1) * ($centre_thing->dim_v->[2] / 2 - $self->dim_v->[2] / 2);
                }
            }
        }

        @pattern_shifts = main::find_stretched_ring_points(
                $rotation_centre,
                1, 
                $start,
                $radius * $self->ringstretchx / $max_stretch,
                $radius * $self->ringstretchy / $max_stretch,
                $angle,
                $self->ringcount,
                );

        if ($self->climb) {
            for my $i (0 .. $#pattern_shifts) {
                $pattern_shifts[$i]->[1] += $i * $self->climb * $grid_cell_size;
            }
        }

    }
    else {
        $log->logdie("bad pattern: " . $self->pattern);
    }
    $self->{render_calc}->{pattern_shifts} = \@pattern_shifts;

    my $base_y = 0;

    # cuboid {{{3
    if ($self->type eq $TT_CUBOID) {

        # set up an array holding all 8 vertices; clockwise bottom, then clockwise top
        $self->{render_calc}->{vertices} = [
            [ 0, $base_y, 0, ],
            [ $dim_v->[0], $base_y, 0, ],
            [ $dim_v->[0], $base_y, $dim_v->[2], ],
            [ 0, $base_y, $dim_v->[2], ],
            [ 0, $dim_v->[1], 0, ],
            [ $dim_v->[0], $dim_v->[1], 0, ],
            [ $dim_v->[0], $dim_v->[1], $dim_v->[2], ],
            [ 0, $dim_v->[1], $dim_v->[2], ],
        ];

    }
    # box {{{3
    elsif ($self->type eq $TT_BOX) {

        my $wall = $self->wall;

        $self->{render_calc}->{vertices} = [

            [ 0, $base_y, 0, ],
            [ $dim_v->[0], $base_y, 0, ],
            [ $dim_v->[0], $base_y, $dim_v->[2], ],
            [ 0, $base_y, $dim_v->[2], ],
            [ 0, $dim_v->[1], 0, ],
            [ $dim_v->[0], $dim_v->[1], 0, ],
            [ $dim_v->[0], $dim_v->[1], $dim_v->[2], ],
            [ 0, $dim_v->[1], $dim_v->[2], ],

            # inner box; could calc via a loop...
            [ 0 + $wall * $grid_cell_size, $base_y, 0 + $wall * $grid_cell_size, ],
            [ $dim_v->[0] - $wall * $grid_cell_size, $base_y, 0 + $wall * $grid_cell_size, ],
            [ $dim_v->[0] - $wall * $grid_cell_size, $base_y, $dim_v->[2] - $wall * $grid_cell_size, ],
            [ 0 + $wall * $grid_cell_size, $base_y, $dim_v->[2] - $wall * $grid_cell_size, ],
            [ 0 + $wall * $grid_cell_size, $dim_v->[1], 0 + $wall * $grid_cell_size, ],
            [ $dim_v->[0] - $wall * $grid_cell_size, $dim_v->[1], 0 + $wall * $grid_cell_size, ],
            [ $dim_v->[0] - $wall * $grid_cell_size, $dim_v->[1], $dim_v->[2] - $wall * $grid_cell_size, ],
            [ 0 + $wall * $grid_cell_size, $dim_v->[1], $dim_v->[2] - $wall * $grid_cell_size, ],

        ];

    }
    # prism {{{3
    elsif ($self->type eq $TT_PRISM) {

        # properties:
        # Sides, Plane, Angle, Size, CustomSize, 

        # we need to define the normals and vertices used by draw_prism.

        my $radius = $dim_v->[$axis_dim->{$self->axis}->{x}] / 2;

        my $angle_change = 360 / $self->sides;

        my $start_angle = 90 - ($angle_change / 2) + $self->angle;

        my @side_points;

        # define the ends
        delete $self->{render_calc}->{end_vertices};
        for my $end (0,1) {

            my $centre_end = [];
#            $centre_end->[$axis_dim->{$self->axis}->{x}] = $radius;
#            $centre_end->[$axis_dim->{$self->axis}->{y}] = $radius;
            $centre_end->[$axis_dim->{$self->axis}->{x}] = $dim_v->[$axis_dim->{$self->axis}->{x}] / 2;
            $centre_end->[$axis_dim->{$self->axis}->{y}] = $dim_v->[$axis_dim->{$self->axis}->{y}] / 2;
            $centre_end->[$self->axis] = ($dim_v->[$self->axis] / 2);

            # move to positive or negative in the dimension of the prism
            $centre_end->[$self->axis] += ($end ? 1 : -1) * ($dim_v->[$self->axis] / 2);

            # the centre point is the first point we need for rendering
            push @{ $self->{render_calc}->{end_vertices}->[$end] }, $centre_end;

            my $end_size_ratio = $end ? $self->ratiotop : $self->ratiobottom;

            # generate points around rim; note that the first point only appears in this list as the final point,
            # since we define the angle change and count to do a full revolution.
            my @rim_points = main::find_stretched_ring_points(
                $centre_end,
                $self->axis,
                $start_angle,
                ($dim_v->[$axis_dim->{$self->axis}->{x}] / 2) * $end_size_ratio,
                ($dim_v->[$axis_dim->{$self->axis}->{y}] / 2) * $end_size_ratio,
                $angle_change,
                $self->sides + 1,
                );

            # we now have the complete list of points around the rim; by combining these lists from
            # both ends we get the quad_strip lists for the side faces.
            if (@side_points) {
                # second loop; mesh them together
                $self->{render_calc}->{side_points} = [ mesh @rim_points, @side_points ];
            }
            else {
                # first loop, just record them
                @side_points = @rim_points;
            }

            # add the rim points
            push @{ $self->{render_calc}->{end_vertices}->[$end] }, @rim_points;

            # we can now use the first 3 points in the vertices list to find the normal to the plane
            if ($end) {
                $self->{render_calc}->{end_normals}->[$end] = [ main::triangle_normal(
                    @{ $self->{render_calc}->{end_vertices}->[$end]->[0] },
                    @{ $self->{render_calc}->{end_vertices}->[$end]->[2] },
                    @{ $self->{render_calc}->{end_vertices}->[$end]->[1] },
                    ) ];
            }
            else {
                $self->{render_calc}->{end_normals}->[$end] = [ main::triangle_normal(
                    @{ $self->{render_calc}->{end_vertices}->[$end]->[0] },
                    @{ $self->{render_calc}->{end_vertices}->[$end]->[1] },
                    @{ $self->{render_calc}->{end_vertices}->[$end]->[2] },
                    ) ];
            }

        }

        # calculate side normals
        $self->{render_calc}->{side_normals} = [];
        for my $index (1 .. scalar @{ $self->{render_calc}->{side_points}} / 2 - 1) {
            push @{ $self->{render_calc}->{side_normals} },
                [ main::triangle_normal(
                    @{ $self->{render_calc}->{side_points}->[ $index * 2 - 1] },
                    @{ $self->{render_calc}->{side_points}->[ $index * 2 - 2] },
                    @{ $self->{render_calc}->{side_points}->[ $index * 2 - 0] },
                )];

        }

    }
    # stairs {{{3
    elsif ($self->type eq $TT_STAIRS) {

        my $steps = $self->{steps};
        my $direction = $self->{direction};
        my $axis = $self->{axis};
        $log->debug("direction = $direction, axis = $axis");

        my $long_axis = ($axis + ($direction < 2 ? 1 : -1)) % 3;
        my $wide_axis = ($axis + ($direction < 2 ? -1 : 1)) % 3;
        $log->debug("long_axis = $long_axis, wide_axis = $wide_axis");

        # steps going against the "natural" direction have a -ve depth; saves
        # using a -1/1 factor.
        my $step_depth = $dim_v->[$long_axis] / $steps * ($direction % 3 ? -1 : 1);
        my $step_height = $dim_v->[$axis] / $steps;
        $log->debug("depth $step_depth, height $step_height");

        # this is the coord of the long dimension which the stairs run up to.
        my $top_side = $direction % 3 ? $dim_v->[$long_axis] : 0;

        # left side coord looking up the stairs
        my $left_side = $direction % 2 ? $dim_v->[$wide_axis] : 0;
        $log->debug("top_side $top_side, left_side $left_side");

        # start the vertex lists
        my $left_side_points = [];

        # origin point; left front of top step
        my @point;

        # origin point; left bottom of top step, ie left bottom of stairs
        $point[$wide_axis] = $left_side;
        $point[$long_axis] = $top_side;
        $point[$axis] = 0;
        push @{ $left_side_points }, [ @point ];

        # left bottom of front step
        $point[$long_axis] = $top_side + $step_depth * $steps;
        push @{ $left_side_points }, [ @point ];

        # make the list of vertexes a real thing so we can look around the end
        my @vertices = (0 .. $steps * 2 + ($steps >= 2 ? 1 : 0));

        if ($steps >= 2) {

            for my $i ( 1 .. $steps ) {

                # top of current step
                $point[$axis] += $step_height;
                push @{ $left_side_points }, [ @point ];

                # bottom of next step
                $point[$long_axis] -= $step_depth;
                push @{ $left_side_points }, [ @point ];
            }
        }
        else {

            # steps == 1 means a ramp

            # the only point we need is the top of the ramp
            $point[$long_axis] = $top_side;
            $point[$axis] = $dim_v->[$axis];
            push @{ $left_side_points }, [ @point ];

        }

        # mirror points to create right side; the current value is either 0 or not,
        # and that 0 was assigned by us so we can test it (ie it won't be 0.0000001)
        my $right_side_points = dclone $left_side_points;
        map { $_->[$wide_axis] = $_->[$wide_axis] ? 0 : $dim_v->[$wide_axis]; } @{ $right_side_points };

        my $sides = $self->{render_calc}->{sides} = [
            $left_side_points,
            $right_side_points,
        ];

        # calculate normals for steps
        $self->{render_calc}->{extrusion_normals} = [];
        my $previous_vertex = undef;

        for my $v (@vertices) {

            unless (defined $previous_vertex) {
                $previous_vertex = $vertices[$#vertices];
            }

            # make the normal from the previous position
            push @{ $self->{render_calc}->{extrusion_normals} }, [ main::triangle_normal(
                @{ $sides->[1]->[$v] },
                @{ $sides->[0]->[$v] },
                @{ $sides->[0]->[$previous_vertex] },
            )];

            $previous_vertex = $v;
        }

        # side normals
        $self->{render_calc}->{side_normals} = [];
        for my $side (0,1) {
            push @{ $self->{render_calc}->{side_normals} }, [ main::triangle_normal(
                @{ $sides->[$side]->[$side] },
                @{ $sides->[$side]->[$side ? 0 : 1] },
                @{ $sides->[$side]->[$#vertices] },
            )];
        }

    }
    # arch {{{3
    elsif ($self->type eq $TT_ARCH) {

        my $legs = $self->{legs};
        my $base = $self->{base};
        my $square = $self->{square};
        my $direction = $self->{direction};
        my $steps = $self->{steps};

        # for now, set axis from direction; can't have arches through the Y axis.
        # No difference b/w north/south or east/west.
        my ($front_axis, $side_axis) = $direction < 2 ? (0,2) : (2,0);

        # $base is the width of the legs in cells, $legs is the height of the legs (if any) in cells.
        # cap these values sensibly; it would be easier to defined them in ratio terms but
        # this would make match other structures to them hard/impossible.
        if ($legs + 1 > ($dim_v->[1] / $grid_cell_size)) {
            $legs = int($dim_v->[1] / $grid_cell_size) - 1;
        }
        if ($base > int(int($dim_v->[$front_axis] / $grid_cell_size) / 2)) {
            $base = int(int($dim_v->[$front_axis] / $grid_cell_size) / 2);
        }

        my $arch_centre = [];
        $arch_centre->[$front_axis] = $dim_v->[$front_axis] / 2;
        $arch_centre->[$side_axis] = 0;
        $arch_centre->[1] = $legs * $grid_cell_size;

        my $count = $steps + 2;
        my $angle_change = 180 / ($steps + 1);
        my $angle_start = 0 - $angle_change;

        my @inner_arch_points = main::find_stretched_ring_points(
            $arch_centre, $side_axis, $angle_start,
                $dim_v->[$front_axis] / 2 - $base * $grid_cell_size, # x size
                $dim_v->[1] - ($legs + $base) * $grid_cell_size, # y size
                $angle_change, $count);

        my @outer_arch_points;
        if ($square) {

            # just do this manually
            my @front_points = ( $dim_v->[$front_axis], $dim_v->[$front_axis], $dim_v->[$front_axis] / 2, 0, 0 );
            my @height_points = ( $legs * $grid_cell_size, $dim_v->[1], $dim_v->[1], $dim_v->[1], $legs * $grid_cell_size );
            my @point;
            for my $i ( 0 .. 4 ) {

                $point[$front_axis] = $front_points[$i];
                $point[$side_axis] = 0;
                $point[1] = $height_points[$i];

                push @outer_arch_points, [ @point ];
            }
        }
        else {

            @outer_arch_points = main::find_stretched_ring_points(
                $arch_centre, $side_axis, $angle_start,
                    $dim_v->[$front_axis] / 2, # x size
                    $dim_v->[1] - $legs * $grid_cell_size, # y size
                    $angle_change, $count);
        }

        if ($legs) {

            my @point;
            $point[$front_axis] = $dim_v->[$front_axis] - $base * $grid_cell_size;
            $point[$side_axis] = 0;
            $point[1] = 0;
            unshift @inner_arch_points, [ @point ];

            $point[$front_axis] = $dim_v->[$front_axis];
            unshift @outer_arch_points, [ @point ];

            $point[$front_axis] = $base * $grid_cell_size;
            push @inner_arch_points, [ @point ];

            $point[$front_axis] = 0;
            push @outer_arch_points, [ @point ];
        }

        $self->{render_calc}->{sides}->[0] = [ @inner_arch_points, reverse @outer_arch_points ];
        $self->{render_calc}->{sides}->[1] = dclone $self->{render_calc}->{sides}->[0]; 
        map { $_->[$side_axis] = $dim_v->[$side_axis]; } @{ $self->{render_calc}->{sides}->[1] };

        # calculate normals for sides
        my $sides = $self->{render_calc}->{sides};
        $self->{render_calc}->{extrusion_normals} = [];
        my $previous_vertex = undef;

        my @vertices = (0 .. $#{ $sides->[0] });
        my ($n1,$n2) = $direction < 2 ? (1,0) : (0,1);

        for my $v (@vertices) {

            unless (defined $previous_vertex) {
                $previous_vertex = $vertices[$#vertices];
            }

            # make the normal from the previous position
            push @{ $self->{render_calc}->{extrusion_normals} }, [ main::triangle_normal(
                @{ $sides->[$n1]->[$v] },
                @{ $sides->[$n2]->[$v] },
                @{ $sides->[0]->[$previous_vertex] },
            )];

            $previous_vertex = $v;
        }

        # side normals
        $self->{render_calc}->{side_normals} = [];
        for my $side (0,1) {
            push @{ $self->{render_calc}->{side_normals} }, [ main::triangle_normal(
                @{ $sides->[$side]->[$side ? $n1 : $n2] },
                @{ $sides->[$side]->[$side ? $n2 : $n1] },
                @{ $sides->[$side]->[$#vertices] },
            )];
        }

    }
    # sphere {{{3
    elsif ($self->type eq $TT_SPHERE) {

        my $steps = $self->{steps};
        my $smooth = $self->{smooth};

        # work out the rings of points; steps is the number of rings, ie a D8 shape has one ring.
        # There are the same number of points around a ring as there is around top to bottom to top.

        my $ring_points = 2 * ($steps + 1);
        my $inter_ring_angle = 180 / ($steps + 1);

        # pole points
        $self->{render_calc}->{poles}->[0] = [ $dim_v->[0] / 2, $dim_v->[1], $dim_v->[2] / 2];
        $self->{render_calc}->{poles}->[1] = [ $dim_v->[0] / 2, 0, $dim_v->[2] / 2];
        
        delete $self->{render_calc}->{rings};

        for my $ring (1 .. $steps) {
            
            # we build from the top ring down; we start the ring elevation angle at 90 - $inter_ring_angle, 
            # and take off another $inter_ring_angle for each ring.
            my $elevation = 90 - $ring * $inter_ring_angle;

            # the elevation will be -ve for the second half of the ring so we get -ve cos values as well.
            # The sin values are used as the offset from half the height of the thing.
            my $ring_y = ($dim_v->[1] / 2) + sin(deg2rad($elevation)) * ($dim_v->[1] / 2);

            my $ring_size = cos(deg2rad($elevation));

            # we can now radiate around the ring center by radiating around the Y axis
            my @ring_points = main::find_stretched_ring_points(
                [ $dim_v->[0] / 2, $ring_y, $dim_v->[2] / 2],
                1, 0, $dim_v->[0] * $ring_size / 2, $dim_v->[2] * $ring_size / 2, $inter_ring_angle, $ring_points);

            push @{ $self->{render_calc}->{rings} }, [ @ring_points ];
        }

    }
    # shape {{{3
    elsif ($self->type eq $TT_SHAPE) {

#        $log->logdie("no shapes!");

        my $shape_name = $self->{shape};
        my $stretch = $self->{stretch};
        my $axis = $self->{axis};
        my $reverse_side_normal = $axis == 2;
        my $reverse_end_normal = $axis == 2;

        # manual normal swap may be required if the shape was drawn CCW
#        $reverse_end_normal = ! $reverse_end_normal if $self->{swaptexture};
        $reverse_side_normal = ! $reverse_side_normal if $self->{swaptexture};

        # yes, if both are swapped they cancel out the change to the normal
        $reverse_side_normal = ! $reverse_side_normal if $self->{swapx};
        $reverse_side_normal = ! $reverse_side_normal if $self->{swapy};

        my $shape = LoadFile("shapes/$shape_name.2d");

        # find conversion factors; x & y here refer to the shape coords
        my ($x_factor, $y_factor);

        # if we stretch, both shape dimensions stretch to fit the target dimensions.
        # Otherwise, we use the min factor.
        $x_factor = $dim_v->[$axis_dim->{$axis}->{x}] / $shape->{max}->{x};
        $y_factor = $dim_v->[$axis_dim->{$axis}->{y}] / $shape->{max}->{y};
        $log->debug("factors: $x_factor, $y_factor");

        unless ($stretch) {

            # map the shape into the thing with the largest possible size still
            # retaining the aspect ratio, ie common factor for both dims

            $x_factor = $y_factor = List::Util::min($x_factor, $y_factor);
        }

        $self->{render_calc}->{sides} = [ [], [] ];

        # triangulation points
        my @scaled_2d_points;

        # shape has a flat list of 2d points between 0 and 1; convert to thing dimensions
        for my $point (0 .. (scalar @{ $shape->{path} }) / 2 - 1) {

            my $raw_x = $shape->{path}->[$point * 2];
            my $raw_y = $shape->{path}->[$point * 2 + 1];

            # do swapping here before we derive any 3d points, and while the value range is still known
            $raw_x = 1 - $raw_x if $self->{swapx};
            $raw_y = 1 - $raw_y if $self->{swapy};

            for my $end (0,1) {

                my $d3_point = [];

                # map the x & y points to a 3d point according to the axis and add the third point along
                # that axis.
                $d3_point->[$axis_dim->{$axis}->{x}] = $raw_x * $x_factor;
                $d3_point->[$axis_dim->{$axis}->{y}] = $raw_y * $y_factor;
                $d3_point->[$axis] = $end ? $dim_v->[$axis] : 0;

                push @{ $self->{render_calc}->{sides}->[$end] }, $d3_point;
            }

            # make a list of scaled 2d points for triangulation
            push @scaled_2d_points, [ $raw_x * $x_factor, $raw_y * $y_factor ];
        }

        $log->debug("scaled_2d_points " . Dumper(\@scaled_2d_points));

        # calculate normals for sides
        $self->{render_calc}->{extrusion_normals} = [];
        my $previous_vertex = undef;

        my $sides = $self->{render_calc}->{sides};
        my @vertices = (0 .. $#{ $sides->[0] } );
        for my $v (@vertices) {

            unless (defined $previous_vertex) {
                $previous_vertex = $vertices[$#vertices];
            }

            # make the normal from the previous position
            push @{ $self->{render_calc}->{extrusion_normals} }, [ main::triangle_normal(
                @{ $sides->[1]->[$v] },
                @{ $sides->[0]->[$v] },
                @{ $sides->[0]->[$previous_vertex] },
            )];

            if ($reverse_side_normal) {
                for my $i (0 .. 2) {
                    $self->{render_calc}->{extrusion_normals}->[$v]->[$i] *= -1;
                }
            }

            $previous_vertex = $v;
        }

        # calculate triangles for ends
        $self->{render_calc}->{end_triangles} = [];

        # don't want the repeated last point
        pop @scaled_2d_points;
        my $polygon = Math::Geometry::Planar->new;
        $log->debug("make polygon");
        eval { 
            $polygon->points(\@scaled_2d_points);
        };
        if ($@) {
            $log->warn("error making polygon: $@");
        }
        else {
            my @triangles;
            $log->debug("triangulate");
            eval {
                @triangles = $polygon->triangulate;
            };
            if ($@) {
                $log->warn("error triangulating: $@");
            }
            else {
                $log->debug("triangles: " . Dumper(\@triangles));

                # we have a list of 2d triangles; convert them to 3d
                for my $end (0,1) {
                    for my $triangle (@triangles) {
                        my $d3_triangle = [];
                        for my $point ( @{ $triangle->points } ) {
                            my $d3_point = [];

                            # map the x & y points to a 3d point according to the axis and add the third point along
                            # that axis.
                            $d3_point->[$axis_dim->{$axis}->{x}] = $point->[0];
                            $d3_point->[$axis_dim->{$axis}->{y}] = $point->[1];
                            $d3_point->[$axis] = $end ? $dim_v->[$axis] : 0;

                            push @{ $d3_triangle }, $d3_point;

                        }
                        push @{ $self->{render_calc}->{end_triangles}->[$end] }, $d3_triangle;
                    }

                    # we can now use the first triangle in the list to find the normal to the plane
                    if ($end) {
                        $self->{render_calc}->{end_normals}->[$end] = [ main::triangle_normal(
                            @{ $self->{render_calc}->{end_triangles}->[$end]->[0]->[0] },
                            @{ $self->{render_calc}->{end_triangles}->[$end]->[0]->[2] },
                            @{ $self->{render_calc}->{end_triangles}->[$end]->[0]->[1] },
                            ) ];
                    }
                    else {
                        $self->{render_calc}->{end_normals}->[$end] = [ main::triangle_normal(
                            @{ $self->{render_calc}->{end_triangles}->[$end]->[0]->[0] },
                            @{ $self->{render_calc}->{end_triangles}->[$end]->[0]->[1] },
                            @{ $self->{render_calc}->{end_triangles}->[$end]->[0]->[2] },
                            ) ];
                    }

                    if ($reverse_end_normal) {
                        for my $i (0 .. 2) {
                            $self->{render_calc}->{end_normals}->[$end]->[$i] *= -1;
                        }
                    }

                }
            }
        }

    }

    # finish {{{3

#                        # test of normal vector calc using Math::VectorReal given 3 vertexes COUNTERCLOCKWISE
#                        my $vector_0 = vector( @{ $self->{render_calc}->{side_points}->[ $index * 2 - 2] } );
#                        my $vector_3 = vector( @{ $self->{render_calc}->{side_points}->[ $index * 2 - 1] } );
#                        my $vector_7 = vector( @{ $self->{render_calc}->{side_points}->[ $index * 2 - 0] } );
#                        my ($n, $d) = plane ( $vector_0, $vector_3, $vector_7, );
#                        $log->info("normal = $n, d = $d");

    wxTheApp->dirty(1);

    return;
}

#*******************************************************************************
# Send the specified vertex to OpenGL and maybe add it to the thing's list, if we're
# showing vertices. Also tracks the real extent of the thing (including the group) if
# we're adjusting.
sub add_vertex { #{{{2
    my ($self, @vertex) = @_;

    unless (defined $vertex[0]) {
        $log->logconfess("null vertex dim");
        return;
    }

    push @{ $self->{render_calc}->{vertex_list} }, [ @vertex ]
        if $self->state & $TS_SHOW_VERTICES;

    glVertex3f(@vertex);

    # if we're adjusting, we want to track the extents of the whole pattern
    if ($self->state & $TS_ADJUSTING) {

        my $current_shift = $self->current_shift;

        foreach my $dim (0 .. 2) {

            # adjust vertex for shift; we've already sent the unshifted values to OpenGL
            $vertex[$dim] += $current_shift->[$dim] if $current_shift;

            # we're initialising to NaN so make sure false condition sets the value.
            if (! ($vertex[$dim] <= $self->pattern_max->[$dim])) {
                $self->pattern_max->[$dim] = $vertex[$dim];
            }
            if (! ($vertex[$dim] >= $self->pattern_min->[$dim])) {
                $self->pattern_min->[$dim] = $vertex[$dim];
            }

        }

    }

    return;
}

#*******************************************************************************
# Compile the specified display list (normal or selection) for this thing.
sub compile { #{{{2
    my ($self, $selection_mode) = @_;

    # start {{{3

    my $app = wxTheApp;
    my $canvas = $app->frame->{canvas};

    # there are system objects (grids, etc) taking the low display list ids,
    # and so we have an offset past these.
    my $display_list_id = $self->id + $main::DL_MAX_OFFSET + ($selection_mode ? 100000 : 0);

    glNewList($display_list_id, GL_COMPILE);

    glPushMatrix();

    if ($self->texture && $self->texture ne 'none' && ! $selection_mode) {
        if (defined (my $texture_id = $canvas->texture_id->{$self->texture})) {
            glEnable(GL_TEXTURE_2D);

            # the texture index is off by one because the property list includes 'none'
            glBindTexture(GL_TEXTURE_2D, $texture_id);
        }
        else {
            $log->warn("texture " . $self->texture . " not recognised, skipped");
            glDisable(GL_TEXTURE_2D);
        }

    }
    else {
        glDisable(GL_TEXTURE_2D);
    }

    # translate so origin is in middle of object, so it rotates
    # around that point.
    glTranslatef(
        $self->offset_v->[0] + $self->dim_v->[0] / 2, 
        $self->offset_v->[1] + $self->dim_v->[1] / 2, 
        $self->offset_v->[2] + $self->dim_v->[2] / 2, 
    );

    if ($self->rotatex) {
        glRotatef($self->rotatex, 1, 0, 0);
    }

    if ($self->rotatey) {
        glRotatef($self->rotatey, 0, 1, 0);
    }

    if ($self->rotatez) {
        glRotatef($self->rotatez, 0, 0, 1);
    }

    # translate back to lower left rear corner
    glTranslatef(
        - $self->dim_v->[0] / 2, 
        - $self->dim_v->[1] / 2, 
        - $self->dim_v->[2] / 2, 
    );

    if ($selection_mode) {

        # color in selection mode comes from id
        glColor3ub( ($self->id & 0xff0000) >> 16, ($self->id & 0xff00) >> 8, $self->id & 0xff, );
    }
    else {

        # are we selected?
        my @color = ($self->state & $TS_SELECTED)
            ? @{ $self->selected_color_v }
            : @{ $self->color_v };

        if ($self->state & $TS_TRANSPARENT) {
            glColor4f( @color, $TRANSPARENT_ALPHA_05 );
        }
        elsif ($self->state & $TS_ADJUSTING) {
            glColor4f( @color, $TRANSPARENT_ALPHA_03 );
        }
        else {
            glColor3f( @color );
        }
    }

#        if ($app->select_info->{selected_index}->{$self->id}) {
##            $log->debug("rendering selected thing $self->{id}");
##            delete $app->select_info->{selected_index}->{$self->id};
#            
#            # convert to HSV and increase saturation
#            my $color = Graphics::ColorObject->new_RGB([ @color ]);
#            my ($h,$s,$v) = @{ $color->as_HSV() };
#
#            # hilite color has V of 1
#            my $hilite_color = Graphics::ColorObject->new_HSV([ $h, $s, 1 ]);
#            @color = @{ $hilite_color->as_RGB() };
#        }

#    my @texture_counts;
    my $grid_cell_size = $self->grid_cell_size;
#    for my $dim (0 .. 2) {
#        $texture_counts[$dim] = $self->dim_v->[$dim] / $grid_cell_size;
#    }
    my $dim_v = $self->dim_v;
    my $tile = $self->texture_infotile;
    my $texture_factor = $self->texture_infofactor;
    my $texture_cell_size = $texture_factor 
        ? $grid_cell_size * 2 ** $texture_factor
        : $grid_cell_size;

    $self->{render_calc}->{vertex_list} = [];

    # initialise pattern extents 
    if ($self->state & $TS_ADJUSTING) {
        $self->pattern_max([ "NaN", "NaN", "NaN" ]);
        $self->pattern_min([ "NaN", "NaN", "NaN" ]);
    }

    # fake a list of patterns so we can always run a loop
    my $pattern_shifts = $self->{render_calc}->{pattern_shifts} || [ 0 ];

    # we need to know the offset performed by OpenGL Translate calls so we can find
    # global extents of the pattern items
    $self->current_shift([]);

    for my $pattern_index ( 0 .. $#{ $pattern_shifts } ) {

        my $pattern_shift = $pattern_shifts->[$pattern_index];

        if ($pattern_shift) {
            glPushMatrix();

            if ($self->pattern eq $TP_RING && $self->orient) {

                # rotate the object against the ring so the object faces the centre

                my $angle = $self->ringangle || (360 / $self->ringcount),

                # translate for shift and so origin is in middle of object, so it rotates
                # around that point.
                glTranslatef(
                    $pattern_shift->[0] + $dim_v->[0] / 2, 
                    $pattern_shift->[1] + $dim_v->[1] / 2, 
                    $pattern_shift->[2] + $dim_v->[2] / 2, 
                );

                glRotatef((360 - $angle) * ($pattern_index + 1), 0, 1, 0);

                # translate back to lower left rear corner
                glTranslatef(
                    - $dim_v->[0] / 2, 
                    - $dim_v->[1] / 2, 
                    - $dim_v->[2] / 2, 
                );
            }
            else {
                glTranslatef( @{ $pattern_shift } );
            }

            # current shift is offset + pattern shift
            for my $dim (0 .. 2) {
                $self->current_shift->[$dim] = $self->offset_v->[$dim] + $pattern_shift->[$dim];
            }

        }
        else {

            # no pattern, so the shift is just the object offset
            $self->current_shift( $self->offset_v );
        }

        if ($self->type eq $TT_CUBOID) {
            #{{{

            my $vertices = $self->{render_calc}->{vertices};

            glBegin(GL_QUADS);

            my @normals = (
                [ -1, 0, 0 ],
                [ 0, 0, 1 ],
                [ 1, 0, 0 ],
                [ 0, 0, -1 ],
                [ 0, -1, 0 ],
                [ 0, 1, 0 ],
            );
            my @sides = (
                [ 0, 4, 7, 3 ],
                [ 3, 7, 6, 2 ],
                [ 2, 6, 5, 1 ],
                [ 1, 5, 4, 0 ],
                [ 3, 2, 1, 0 ],
                [ 4, 5, 6, 7 ],
            );

            my @texture_coords = (
                [ 2, 1 ],
                [ 0, 1 ],
                [ 2, 1 ],
                [ 0, 1 ],
                [ 0, 2 ],
                [ 0, 2 ],
            );

            for my $side (0 .. $#sides) {

                glNormal3fv_p( @{ $normals[$side] });
                for my $vertex ( @{ $sides[$side] }) {
                    if ($self->texture && $self->texture ne 'none') {

                        # hack to flip top texture
                        my $flip = $side == 5 ? -1 : 1;

                        if ($tile) {

                            unless (defined $vertices->[$vertex]->[$texture_coords[$side]->[0]]) {
                                $log->info("no vertex $vertex for side $side on object " . $self->name);
                            }
                            glTexCoord2f($vertices->[$vertex]->[$texture_coords[$side]->[0]] / $texture_cell_size,
                                $flip * $vertices->[$vertex]->[$texture_coords[$side]->[1]] / $texture_cell_size);
                        }
                        elsif ($dim_v->[$texture_coords[$side]->[0]] && $dim_v->[$texture_coords[$side]->[1]]) {
                            glTexCoord2f($vertices->[$vertex]->[$texture_coords[$side]->[0]] / $dim_v->[$texture_coords[$side]->[0]], 
                                $flip * $vertices->[$vertex]->[$texture_coords[$side]->[1]] / $dim_v->[$texture_coords[$side]->[1]]);
                        }
                    }
                    $self->add_vertex( @{ $vertices->[$vertex] } );
                }

            }

            glEnd();

            #}}}
        }
        elsif ($self->type eq $TT_BOX) {
            #{{{

            my $vertices = $self->{render_calc}->{vertices};

            glBegin(GL_QUADS);
            my @normals = (
                [ -1, 0, 0 ],
                [ 0, 0, 1 ],
                [ 1, 0, 0 ],
                [ 0, 0, -1 ],
            );
            my @sides = (
                [ 0, 4, 7, 3 ],
                [ 3, 7, 6, 2 ],
                [ 2, 6, 5, 1 ],
                [ 1, 5, 4, 0 ],
            );

            my @texture_coords = (2,0,2,0);

            for my $i (0 .. 3) {

                # draw the outer wall
                glNormal3fv_p( @{ $normals[$i] });
                for my $vertex ( @{ $sides[$i] }) {
                    if ($tile) {
                        glTexCoord2f($vertices->[$vertex]->[$texture_coords[$i]] / $texture_cell_size, $vertices->[$vertex]->[1] / $texture_cell_size);
                    }
                    elsif ($dim_v->[$texture_coords[$i]] && $dim_v->[1]) {
                        glTexCoord2f($vertices->[$vertex]->[$texture_coords[$i]] / $dim_v->[$texture_coords[$i]], $vertices->[$vertex]->[1] / $dim_v->[1]);
                    }
                    $self->add_vertex( @{ $vertices->[$vertex] } );
                }

                # draw the inner wall
                glNormal3fv_p( @{ $normals[ ($i + 2) % 4] });
                for my $vertex ( @{ $sides[$i] }) {
                    if ($tile) {
                        glTexCoord2f($vertices->[$vertex + 8]->[$texture_coords[$i]] / $texture_cell_size, $vertices->[$vertex + 8]->[1] / $texture_cell_size);
                    }
                    elsif ($dim_v->[$texture_coords[$i]] && $dim_v->[1]) {
                        glTexCoord2f($vertices->[$vertex + 8]->[$texture_coords[$i]] / $dim_v->[$texture_coords[$i]], $vertices->[$vertex]->[1] / $dim_v->[1]);
                    }
                    $self->add_vertex( @{ $vertices->[$vertex + 8] } );
                }

            }

            glEnd();

            glBegin(GL_TRIANGLE_STRIP);

            # top
            glNormal3f(0,1,0);
            for (12, 4, 13, 5, 14, 6, 15, 7, 12, 4, ) {
                if ($tile) {
                    glTexCoord2f($vertices->[$_]->[0] / $texture_cell_size, -$vertices->[$_]->[2] / $texture_cell_size);
                }
                else {
                    glTexCoord2f($vertices->[$_]->[0] / $dim_v->[0], -$vertices->[$_]->[2] / $dim_v->[2]);
                }
                $self->add_vertex( @{ $vertices->[$_] } );
            }

            # bottom
            glNormal3f(0,-1,0);
            for (11, 3, 10, 2, 9, 1, 8, 0, 11, 3 ) {
                $self->add_vertex( @{ $vertices->[$_] } );
            }

            glEnd();

            #}}}
        }
        elsif ($self->type eq $TT_PRISM) {
            #{{{

            my $calc_props = $self->{render_calc};

            for my $end (0,1) {

                # done as triangle fans
                glBegin(GL_TRIANGLE_FAN);

                # normal away from end
                glNormal3f(@{ $calc_props->{end_normals}->[$end] });

                # list of vertices; centre of end, first rim point, then around the rim and repeat the first point
                for my $vertex (@{ $calc_props->{end_vertices}->[$end] }) {
                    if ($tile) {
                        glTexCoord2f($vertex->[0] / $texture_cell_size, -$vertex->[2] / $texture_cell_size);
                    }
                    else {
                        glTexCoord2f($vertex->[0] / $dim_v->[0], -$vertex->[2] / $dim_v->[2]);
                    }
                    $self->add_vertex( @{ $vertex } );
                }

                # end end
                glEnd();
            }

            # draw sides

            if ($self->smooth) {

                # first two points start the first side, remaining pairs each complete a side and are preceded by a normal
                glBegin(GL_QUAD_STRIP);
                glNormal3f(@{ $calc_props->{side_normals}->[$#{ $calc_props->{side_normals} } ] });
                glTexCoord2f(1,1);
                $self->add_vertex( @{ $calc_props->{side_points}->[0] } );
                glTexCoord2f(1,0);
                $self->add_vertex( @{ $calc_props->{side_points}->[1] } );

                # smooth prism side textures ignore tile, since the shared vertices used
                # by GL_QUAD_STRIP can't be shared sensibly for texture coords.
                # Instead, we paint the entire texture around the prism.

                # Each side represents 1 / side proportion of the texture.
                my $side_span = 1 / $self->sides;

                for my $index ( 0 .. $#{ $calc_props->{side_normals} } ) {
                    glNormal3f(@{ $calc_props->{side_normals}->[$index] });
                    glTexCoord2f(1 - ($index + 1) * $side_span,1);
                    $self->add_vertex( @{ $calc_props->{side_points}->[($index + 1) * 2] } );
                    glTexCoord2f(1 - ($index + 1) * $side_span,0);
                    $self->add_vertex( @{ $calc_props->{side_points}->[($index + 1) * 2 + 1] } );
                }
            }
            else {

                # texture calculations need the length of a side
                my $side_len = main::vector_length( @{ $calc_props->{side_points}->[0] }, @{ $calc_props->{side_points}->[2] });

                # one vector and one quad per side
                glBegin(GL_QUADS);
                for my $index ( 0 .. $#{ $calc_props->{side_normals} } ) {
                    glNormal3f(@{ $calc_props->{side_normals}->[$index] });
                    if ($tile) {
                        glTexCoord2f($side_len / $texture_cell_size, $dim_v->[1] / $texture_cell_size);
                    }
                    else {
                        glTexCoord2f(1,1);
                    }
                    $self->add_vertex( @{ $calc_props->{side_points}->[$index * 2] } );
                    if ($tile) {
                        glTexCoord2f($side_len / $texture_cell_size, 0);
                    }
                    else {
                        glTexCoord2f(1,0);
                    }
                    $self->add_vertex( @{ $calc_props->{side_points}->[$index * 2 + 1] } );
                    glTexCoord2f(0,0);
                    $self->add_vertex( @{ $calc_props->{side_points}->[($index + 1) * 2 + 1] } );
                    if ($tile) {
                        glTexCoord2f(0, $dim_v->[1] / $texture_cell_size);
                    }
                    else {
                        glTexCoord2f(0,1);
                    }
                    $self->add_vertex( @{ $calc_props->{side_points}->[($index + 1) * 2] } );
                }
            }

            glEnd();
            #}}}
        }
        elsif ($self->type eq $TT_STAIRS) {
            #{{{

            my $steps = $self->{steps};
            my $sides = $self->{render_calc}->{sides};
            my $direction = $self->{direction};
            my $axis = $self->{axis};
            my $long_axis = ($axis + ($direction < 2 ? 1 : -1)) % 3;

            # make the list of vertexes a real thing so we can look around the end
            my @vertices = (0 .. $#{ $sides->[0] } );

            $self->compile_extrusion_sides(\@vertices);

            # sides

            glBegin(GL_TRIANGLES);

            my $x_len = main::vector_length( @{ $sides->[0]->[0] }, @{ $sides->[0]->[1] });
            my $y_len = main::vector_length( @{ $sides->[0]->[0] }, @{ $sides->[0]->[$#vertices] });

            for my $side (0,1) {

                my $draw_side_triangle = sub {
                    my (@points) = @_;

                    for my $i (0 .. 2) {
                        if ($y_len > 0) {
                            if ($tile) {
                                glTexCoord2f($sides->[$side]->[$points[$i]]->[$long_axis] / $texture_cell_size,
                                    $sides->[$side]->[$points[$i]]->[1] / $texture_cell_size );
                            }
                            else {
                                glTexCoord2f($sides->[$side]->[$points[$i]]->[$long_axis] / $x_len,
                                    $sides->[$side]->[$points[$i]]->[1] / $y_len);
                            }
                        }
                        $self->add_vertex( @{ $sides->[$side]->[$points[$i]] } );
                    }

                };

                glNormal3f( @{ $self->{render_calc}->{side_normals}->[$side] } );

                $draw_side_triangle->(0, 1, $#vertices);

                if ($steps >= 2) {
                    for my $step (0 .. $steps - 1) {
                        $draw_side_triangle->(map { $step * 2 + $_ } (2,3,1));
                    }
                }
            }

            glEnd();
            #}}}
        }
        elsif ($self->type eq $TT_ARCH) {
            #{{{

            my $legs = $self->{legs};
            my $square = $self->{square};
            my $base = $self->{base};
            my $direction = $self->{direction};
            my $steps = $self->{steps};
            my $sides = $self->{render_calc}->{sides};
            my $extrusion_normals = $self->{render_calc}->{extrusion_normals};
            my ($front_axis, $side_axis) = $direction < 2 ? (0,2) : (2,0);

            my @vertices = (0 .. $#{ $sides->[0] } );

            $self->compile_extrusion_sides(\@vertices);

            # sides

            my $x_len = $dim_v->[$front_axis];
            my $y_len = $dim_v->[1];

            for my $side (0,1) {

                glNormal3f( @{ $self->{render_calc}->{side_normals}->[$side] } );

                if ($square) {

                    # find 3 points along top
                    my $top_dec = $legs ? 2 : 1;
                    my @top = map { $_ - $top_dec } ($#vertices - 2 .. $#vertices);

                    # find inner arch
                    my @arch = (0 .. $steps + 2 + ($legs ? 2 : 0) - 1);

                    my @fans;

                    if ($steps % 2) {

                        # easy one; symmetrical arch, ie we have a center peak

                        # reverse order after the first point to reverse winding
                        @fans = (
                            [ $top[2], $#vertices, 0 .. $#arch / 2, $top[1], ],
                            [ $top[0], $top[1], $#arch / 2 .. $#arch + 1, ],
                        );

                    }
                    else {

                        # trickier; first fan goes up the arch side, then to other
                        # side of (inner) arch top, then to other side of actual arch top.
                        # second fan just does side of inner arch
                        @fans = (
                            [ $top[2], $#vertices, 0 .. (int($#arch / 2) + 1), $top[0], ],
                            [ $top[0], int($#arch / 2) + 1 .. $#arch + 1, ],
                        );
                    }

                    for my $fan (@fans) {

                        glBegin(GL_TRIANGLE_FAN);

                        for my $v ( @{ $fan } ) {
                            if ($y_len > 0) {
                                if ($tile) {
                                    glTexCoord2f($sides->[$side]->[$v]->[$front_axis] / $texture_cell_size,
                                        $sides->[$side]->[$v]->[1] / $texture_cell_size );
                                }
                                else {
                                    glTexCoord2f($sides->[$side]->[$v]->[$front_axis] / $x_len,
                                        $sides->[$side]->[$v]->[1] / $y_len );
                                }
                            }
                            $self->add_vertex( @{ $sides->[$side]->[$v] } );
                        }

                        glEnd();
                    }

                }
                else {

                    glBegin(GL_TRIANGLE_STRIP);

                    # the vertices are listed inner then outer, with the outer appended in
                    # reverse order. To draw the sides with inner/outer pairs making triangle strips,
                    # we need inner@0, outer@last, inner@1, outer@last-1, etc.
                    for my $v (0 .. int($#vertices / 2)) {
                        for my $v2 ($v, $#vertices - $v) {
                            if ($y_len > 0) {
                                if ($tile) {
                                    glTexCoord2f($sides->[$side]->[$v2]->[$front_axis] / $texture_cell_size,
                                        $sides->[$side]->[$v2]->[1] / $texture_cell_size );
                                }
                                else {
                                    glTexCoord2f($sides->[$side]->[$v2]->[$front_axis] / $x_len,
                                        $sides->[$side]->[$v2]->[1] / $y_len );
                                }
                            }
                            $self->add_vertex( @{ $sides->[$side]->[$v2] } );
                        }
                    }
                    glEnd();
                }

            }

            #}}}
        }
        elsif ($self->type eq $TT_SPHERE) {
            #{{{

            # extracted so we can use it for vertex markers
            $self->compile_sphere;

            #}}}
        }
        elsif ($self->type eq $TT_SHAPE) {
            #{{{

            my $sides = $self->{render_calc}->{sides};
            my $axis = $self->{axis};

            # make the list of vertexes a real thing so we can look around the end
            my @vertices = (0 .. $#{ $sides->[0] } );

            $self->compile_extrusion_sides(\@vertices);

            # ends

            glBegin(GL_TRIANGLES);

            for my $end (0,1) {

                glNormal3f( @{ $self->{render_calc}->{end_normals}->[$end] } );

                for my $triangle ( @{ $self->{render_calc}->{end_triangles}->[$end] } ) {
                    for my $point (0 .. 2) {

                        if ($tile) {
                            glTexCoord2f($triangle->[$point]->[0] / $texture_cell_size, -$triangle->[$point]->[2] / $texture_cell_size);
                        }
                        else {
                            glTexCoord2f($triangle->[$point]->[0] / $dim_v->[0], -$triangle->[$point]->[2] / $dim_v->[2]);
                        }
                        $self->add_vertex ( @{ $triangle->[$point] } );
                    }
                }

            }

            glEnd();
            #}}}
        }
        else {
            $log->warn("don't know how to render type " . $self->type);
        }

        glPopMatrix() if $pattern_shift;

    }

    # finish {{{3

    # do we display vertex markers?
    if ($self->state & $TS_SHOW_VERTICES) {

        # we are already translated to the thing's offset; each vertex in the list
        # will now be translated to, and we'll add a sphere at that location (the 
        # vertex sphere is built to render around the current origin).

        # we want a general translation by half the sphere size 
        # so the spheres are centered around the vertices
        glPushMatrix;
        glTranslatef( map { - $app->vertex_sphere->dim_v->[$_] / 2 } (0,1,2));

        # we need indexes for selection colors
        for my $index ( 0 .. $#{ $self->{render_calc}->{vertex_list} } ) {

            my $vertex = $self->{render_calc}->{vertex_list}->[$index];

            glPushMatrix;

            glTranslatef( @{ $vertex } );

            if ($selection_mode) {

                # make sure the selector id (red byte) is above the grid ids, which
                # live in the bottom 99 non-zero red values.

                # the way to fix this is to use a list of things with vertices currently displayed,
                # and so the limit will become 150 of things with vertices displayed, not 150 things in total
                $log->logdie("have to improve vertex thing identification")
                    if ($self->id > 150);

                glColor3ub( $self->id + 99, ($index & 0xff00) >> 8, $index & 0xff, );
            }
            else {
                glColor3f( @{ $app->vertex_sphere->color_v } );
            }

            # note that compile means "compile into a GL display list"; the vertex 
            # calculations have already been done.
            $app->vertex_sphere->compile_sphere;

            glPopMatrix;
        }

        glPopMatrix;
    }

    # pop off offset translation; anything after here happens in absolute coords
    glPopMatrix;

    # we need to restore rotation matrix but not translation matrix for the adjustment box
    glPushMatrix;

    # translate so origin is in middle of object, so it rotates
    # around that point.
    glTranslatef(
        $self->offset_v->[0] + $self->dim_v->[0] / 2, 
        $self->offset_v->[1] + $self->dim_v->[1] / 2, 
        $self->offset_v->[2] + $self->dim_v->[2] / 2, 
    );

    if ($self->rotatex) {
        glRotatef($self->rotatex, 1, 0, 0);
    }

    if ($self->rotatey) {
        glRotatef($self->rotatey, 0, 1, 0);
    }

    if ($self->rotatez) {
        glRotatef($self->rotatez, 0, 0, 1);
    }

    # translate back to origin so absolute coords work
    glTranslatef(
        - ($self->offset_v->[0] + $self->dim_v->[0] / 2),
        - ($self->offset_v->[1] + $self->dim_v->[1] / 2),
        - ($self->offset_v->[2] + $self->dim_v->[2] / 2),
    );

    if ($self->state & $TS_ADJUSTING) {
        $self->display_adjustment_box($selection_mode);
    }

    glPopMatrix;

    # this ends the compiled object; anything after here will be identified as a
    # different object.
    glEndList();

#    if ($self->state & $TS_WIREFRAME) {
#        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
#    }

    return;
}

#*******************************************************************************
# use pattern extents to display adjustment box; these are absolute coords
# so we've popped off the offset translation.
sub display_adjustment_box { #{{{2
    my ($self, $selection_mode) = @_;

    for my $dim (0 .. 2) {
#        $log->debug("pattern_min $dim: " . $self->pattern_min->[$dim]);
#        $log->debug("pattern_max $dim: " . $self->pattern_max->[$dim]);

        # grow the box minimally so we don't have coincident planes with items on
        # the extremities of the box.
        $self->pattern_min->[$dim] -= 0.001;
        $self->pattern_max->[$dim] += 0.001;
    }

    # Each side of the adjustment box is drawn with 9 segments, 4 corner, 4 side & middle.
    # We need to cope with one of the three dimensions being 0, since this is used to 
    # find the original base area for new objects. This will result in only side being
    # drawn; four of the six collapse with the 0 dim and we don't draw the max side
    # for the two non-zero dims since it would be identical to the min side.
    # We draw in either normal mode, where every type of handle (middle, side, corner)
    # is colored differently, or selection mode, where every segment (6 x 9 = 54) has
    # a unique color. We treat the adjustment as a grid with display_type_id of 3 for 
    # purposes of color selection.

    # we have absolute coords for the adjustment box.
    my @min = @{ $self->pattern_min };
    my @max = @{ $self->pattern_max };

    if ($max[0] > $min[0] && $max[1] > $min[1] && $max[2] > $min[2]) {

        # all three dims non-zero, display a box
        for my $dim (0 .. 2) {

            # each dimension provides 2 sides, min and max. The dimensions of the
            # side are provided by the other 2 dims.
            # We can work out sizes at this point, they're the same for both sides in this dimension.
            my $handle_ratio = 0.25;

            # these are the corner handles
            my @handle_sizes;
            my @far_crosslines;
            my @side_lengths;
            for my $other_dim (0,1) {
                $side_lengths[$other_dim] = $max[ $other_dims[$dim]->[$other_dim] ] - $min[ $other_dims[$dim]->[$other_dim] ];
                $handle_sizes[$other_dim] = $side_lengths[$other_dim] * $handle_ratio,
                $far_crosslines[$other_dim] = $side_lengths[$other_dim] - $handle_sizes[$other_dim];
            }

            # We'll be translating to the handle position to draw these handles so we can 
            # work out the vertices just based on their size.
            my @corner_vertices = main::get_rectangle_vertices([0,0,0], $dim, @handle_sizes);

            # side handles; two pairs here since they have different sizes
            my @side_handle_vertices;
#            for my $pair (0,1) {
                $side_handle_vertices[0] = [ main::get_rectangle_vertices([0,0,0], $dim, $side_lengths[0] - $handle_sizes[0] * 2, $handle_sizes[1]) ];
                $side_handle_vertices[1] = [ main::get_rectangle_vertices([0,0,0], $dim, $handle_sizes[0], $side_lengths[1] - $handle_sizes[1] * 2 ) ];
#                $side_handle_vertices[$pair] = [ main::get_rectangle_vertices([0,0,0], $dim, $side_lengths[$pair] - $handle_sizes[$pair] * 2, $handle_sizes[$pair ? 0 : 1]) ];
#            }

            # middle (of side) vertices
            my @middle_vertices = main::get_rectangle_vertices([0,0,0], $dim, map { $side_lengths[$_] - $handle_sizes[$_] * 2 } (0,1));

            for my $side (0 .. 1) {


                my @base_vertex = @min;
                
                $base_vertex[$dim] = $max[$dim] if $side;

                # corner handles
                my @handle_bases = main::get_rectangle_vertices(\@base_vertex, $dim, @far_crosslines);
                my $index = 0;
                for my $handle_base (@handle_bases) {

                    $self->set_adjustment_segment_color($selection_mode, 'corner', $dim, $side, $index++);

                    glPushMatrix;
                    glTranslatef(@{ $handle_base });
                    glBegin(GL_QUADS);

                    for my $vertex (@corner_vertices) {
                        glVertex3f( @{ $vertex });
                    }

                    glEnd();
                    glPopMatrix;
                }

                # side handles
                for my $pair (0,1) {

                    # we do two translations in sequence here
                    glPushMatrix;

                    # translate from origin to base of side
                    glTranslatef(@base_vertex);

                    # translate from base of side to base of side handle
                    my @zero_vertex = (0,0,0);
                    $zero_vertex[ $other_dims[$dim]->[$pair] ] = $handle_sizes[$pair];
                    glTranslatef(@zero_vertex);

                    for my $edge (0,1) {

                        $self->set_adjustment_segment_color($selection_mode, 'side', $dim, $side, $pair << 1 | $edge);

                        glBegin(GL_QUADS);
                        for my $vertex (@{ $side_handle_vertices[$pair] }) {
                            glVertex3f( @{ $vertex });
                        }
                        glEnd();

                        # between edges, don't pop matrix, translate again for second handle
                        unless ($edge) {
                            @zero_vertex = (0,0,0);
                            $zero_vertex[ $other_dims[$dim]->[$pair ? 0 : 1] ] = $far_crosslines[$pair ? 0 : 1];
                            glTranslatef(@zero_vertex);
                        }
                    }

                    # restore to absolute coords
                    glPopMatrix;
                }

                # middle handles

                # adjust base of object to be base of this handle (we don't need base of object any more)
                # and translate to there
                for my $other_dim (0,1) {
                    $base_vertex[ $other_dims[$dim]->[$other_dim] ] += $handle_sizes[$other_dim];
                }
                glPushMatrix;
                glTranslatef(@base_vertex);

                $self->set_adjustment_segment_color($selection_mode, 'middle', $dim, $side, 0);

                glBegin(GL_QUADS);
                for my $vertex (@middle_vertices) {
                    glVertex3f( @{ $vertex });
                }
                glEnd();
                glPopMatrix;

            }
        }

    }
    else {

        # display a square in the two non-zero dims

    }

    return;
}

#*******************************************************************************
sub set_adjustment_segment_color { #{{{2
    my ($self, $selection_mode, $segment_type, $dim, $side, $index) = @_;

    # colors for middle, side, corner segments
    my $segment_color = {
        middle => [ main::hex_color_to_floats('#fcfc80') ],
        side => [ main::hex_color_to_floats('#e8d04c') ],
        corner => [ main::hex_color_to_floats('#d0a014') ],
        hilite => [ main::hex_color_to_floats('#ff0000') ],
    };

    # id bitmaps for segments
    my $segment_type_id = {
        middle => 0x10,
        side => 0x08,
        corner => 0x04,
    };

    # DDSTTTII
    my $segment_id = ($dim << 6) | ($side << 5) | $segment_type_id->{$segment_type} | $index++;

    if ($selection_mode) {

        # color in selection mode comes from the info we need to id this handle
        glColor3ub( 3, 0, $segment_id);
    }
    else {

        # color is segment type, unless we're the hilited segment
        glColor4f( @{ $segment_color->{$segment_id == wxTheApp->current_segment_id ? 'hilite' : $segment_type} }, $TRANSPARENT_ALPHA_05 );
    }

    return;
}

#*******************************************************************************
sub decode_segment_id { #{{{2
    my ($self, $segment_id) = @_;

    my $dim = ($segment_id & 0xC0) >> 6;
    my $side = ($segment_id & 0x20) >> 5;
    my $type = ($segment_id & 0x1C);
    my $index = ($segment_id & 0x03);

    return ($dim, $side, $type, $index);
}

#*******************************************************************************
sub compile_sphere { #{{{2
    my ($self) = @_;

    my $steps = $self->{steps};
    my $smooth = $self->{smooth};
    my $tile = $self->texture_infotile;
    my $grid_cell_size = $self->{grid_cell_size};
    my $texture_factor = $self->texture_infofactor;
    my $texture_cell_size = $texture_factor 
        ? $grid_cell_size * 2 ** $texture_factor
        : $grid_cell_size;
    my $dim_v = $self->{dim_v};

    my $ring_points = 2 * ($steps + 1);

    my $centre = [ $dim_v->[0] / 2, $dim_v->[1] / 2, $dim_v->[2] / 2 ];

    # do the poles as triangle fans; the first and last rings (which may be the same)
    # give us the rim points.
    for my $pole (0,1) {
        glBegin(GL_TRIANGLE_FAN);

        glNormal3f( map { $self->{render_calc}->{poles}->[$pole]->[$_] - $centre->[$_] } (0 .. 2));
        glTexCoord2f(0.5,$pole);
        $self->add_vertex( @{ $self->{render_calc}->{poles}->[$pole] } );

        # top or bottom pole?
        my $pole_ring_index = $pole ? $steps - 1 : 0;

        # note we need the starting point twice
        my @points = (0 .. $#{ $self->{render_calc}->{rings}->[$pole_ring_index] }, 0);
        for my $index (0 .. $#points) {
            my $point = $points[$index];

            glNormal3f( map { $self->{render_calc}->{rings}->[$pole_ring_index]->[$point]->[$_] - $centre->[$_] } (0 .. 2));

# doing tiled textures around a triangle fan doesn't work, since the end of one triangle is the start
# of the next, and these should have different texture coords. Need to convert to simple triangles to support this,
# low-priority TODO
#            if ($tile) {
#                glTexCoord2f($self->{render_calc}->{poles}->[$pole]->[0] / $texture_cell_size, $self->{render_calc}->{poles}->[$pole]->[2] / $texture_cell_size);
#            }
#            else {
                glTexCoord2f($index / $#points, $pole ? 0 : 1);
#            }
            $self->add_vertex( @{ $self->{render_calc}->{rings}->[$pole_ring_index]->[$point] } );
        }
        glEnd();
    }

    # now do the triangle strips that join each ring

    # express points around the ring and back to the start as a list
    # so we can use an index to this list to wrap a texture around
    # the whole sphere
    my @points = (0 .. $ring_points - 1, 0);

    # similarly, find the maximum ring for texture wrapping down all rings
    my $max_ring = $#{ $self->{render_calc}->{rings} };

    for my $strip (2 .. $steps) {

        my $upper_ring = $strip - 2;
        my $lower_ring = $upper_ring + 1;

        my $side_len = main::vector_length( @{ $self->{render_calc}->{rings}->[$upper_ring]->[0] }, @{ $self->{render_calc}->{rings}->[$lower_ring]->[0] });

        glBegin(GL_TRIANGLE_STRIP);
        for my $index (0 .. $#points) {

            my $point = $points[$index];

            # the normal for every point on a sphere is point - centre.
            glNormal3f( map { $self->{render_calc}->{rings}->[$upper_ring]->[$point]->[$_] - $centre->[$_] } (0 .. 2));
            if ($tile) {
                glTexCoord2f(($point % 2) ? $side_len / $texture_cell_size : 0, $dim_v->[1] / $texture_cell_size);
            }
            else {
                glTexCoord2f($index / $#points, -$upper_ring / $max_ring);
            }
            $self->add_vertex( @{ $self->{render_calc}->{rings}->[$upper_ring]->[$point] } );

            glNormal3f( map { $self->{render_calc}->{rings}->[$lower_ring]->[$point]->[$_] - $centre->[$_] } (0 .. 2));
            if ($tile) {
                glTexCoord2f(($point % 2) ? $side_len / $texture_cell_size : 0, 0);
            }
            else {
                glTexCoord2f($index / $#points, -$lower_ring / $max_ring);
            }
            $self->add_vertex( @{ $self->{render_calc}->{rings}->[$lower_ring]->[$point] } );
        }
        glEnd;

    }

    return;
}

#*******************************************************************************
# Compile the sides of an extrusion-type object, ie the parallel facets. Sides
# will have individual normals, ie faceted shading rather than progressive shading.
sub compile_extrusion_sides { #{{{2
    my ($self, $vertices) = @_;

    my $extrusion_normals = $self->{render_calc}->{extrusion_normals};
    my $sides = $self->{render_calc}->{sides};
    my $dim_v = $self->dim_v;
    my $grid_cell_size = $self->grid_cell_size;
    my $texture_factor = $self->texture_infofactor;
    my $texture_cell_size = $texture_factor 
        ? $grid_cell_size * 2 ** $texture_factor
        : $grid_cell_size;
    my $tile = $self->texture_infotile;
    my $y_start = 0;
    my $prev_y_factor;

    glBegin(GL_TRIANGLES);

    my $previous_vertex = undef;
    for my $v (@{ $vertices }) {

        # if we haven't got a previous vertex, look backward past
        # the start of the list to the end.
        unless (defined $previous_vertex) {
            $previous_vertex = $vertices->[$#{ $vertices }];
        }

        glNormal3f( @{ $extrusion_normals->[$v] });

        # this is the length of this face along the extrusion; it's constant, but we don't know what axis 
        # the extrusion runs along.
        my $x_len = main::vector_length( @{  $sides->[0]->[$v] }, @{ $sides->[1]->[$v] } );

        # this is the length of this face across the extrusion
        my $y_len = main::vector_length( @{  $sides->[0]->[$v] }, @{ $sides->[0]->[$previous_vertex] } );

        # Keeping this as an anon sub works for us by binding a heap of variables
        # without passing them in, plus we can have prev_y_factor act as a static.
        # Performance isn't really a big issue given our use of display lists.
        my $draw_texture_coord_sub = sub {
            my ($x_factor, $y_factor) = @_;

            if ($tile) {
                my $y_end = $y_start + $y_factor * $y_len / $texture_cell_size;
                glTexCoord2f($x_factor * $x_len / $texture_cell_size, $y_end);
                if (defined $prev_y_factor && $prev_y_factor != $y_factor) {
                    $y_start = $y_end - int($y_end);
                }
                $prev_y_factor = $y_factor;
            }
            else {
                glTexCoord2f($x_factor, $y_factor);
            }
        };

        # draw two triangles to make this side 
        $draw_texture_coord_sub->(0,1);
        glVertex3f( @{ $sides->[0]->[$v] } );
        $draw_texture_coord_sub->(1,1);
        glVertex3f( @{ $sides->[1]->[$v] } );
        $draw_texture_coord_sub->(1,0);
        glVertex3f( @{ $sides->[1]->[$previous_vertex] } );

        glVertex3f( @{ $sides->[1]->[$previous_vertex] } );
        $draw_texture_coord_sub->(0,0);
        glVertex3f( @{ $sides->[0]->[$previous_vertex] } );
        $draw_texture_coord_sub->(0,1);
        glVertex3f( @{ $sides->[0]->[$v] } );

        $previous_vertex = $v;
    }

    glEnd();

    return;
}

#*******************************************************************************
sub render { #{{{2
    my ($self) = @_;

    return if $self->state & ($TS_HIDDEN | $TS_NO_RENDER);

    my $app = wxTheApp;
    my $canvas = $app->frame->{canvas};

    # polygon mode not saved in display list, can't compile it
    if ($self->state & $TS_WIREFRAME && ! $canvas->selection_mode) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    }

    if ($self->needs_compile) {

        # compile normal & selection display lists
        $self->compile(0);
        $self->compile(1);
        $self->needs_compile(0);
    }

    glCallList($self->id + $main::DL_MAX_OFFSET + ($canvas->selection_mode ? 100000 : 0));

    if ($self->state & $TS_WIREFRAME && ! $canvas->selection_mode) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    }

    return;
}

################################################################################

1;
