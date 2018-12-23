use strict;
package WxBuildModel;
use Moose;
use Log::Log4perl qw(get_logger);

use Data::Dumper;
use YAML qw(LoadFile DumpFile);
use List::Util qw(min max);
use List::MoreUtils qw(firstidx mesh);
use Storable qw(dclone);
use Math::Trig;

my $log = get_logger();

has 'filename' => ( 
    isa => 'Str', 
    is => 'rw',
);

has 'cursor_pos' => (
    isa => 'ArrayRef', 
    is => 'rw',
    default => sub { [10,10,10] },
);

has 'things' => (
    isa => 'ArrayRef', 
    is => 'rw',
    default => sub { [] },
);

has 'view_setting' => (
    isa => 'HashRef', 
    is => 'ro',
    default => sub { {} },
);


__PACKAGE__->meta->make_immutable;

sub BUILD {
    my ($self, $params) = @_;

    $log->debug("BUILD:" . Dumper($params));

    if ($params->{file}) {
        $self->open_from_file($params->{file});
    }
}

# vertices of items under construction; 
my @vertices = ();
my $new_vertex = {};

my @things;
my $new_thing;

################################################################################

sub
numeric 
{
    $a <=> $b;
}
sub
rev_numeric 
{
    $b <=> $a;
}

################################################################################

# Copied from Math::Geometry to avoid import problems

sub vector_product  {
    my($a,$b,$c,$d,$e,$f)=@_;
    return($b*$f-$c*$e,$c*$d-$a*$f,$a*$e-$b*$d);
}

sub triangle_normal {
    my(($ax,$ay,$az),($bx,$by,$bz),($cx,$cy,$cz))=@_;
    my(@AB)=($bx-$ax,$by-$ay,$bz-$az);
    my(@AC)=($cx-$ax,$cy-$ay,$cz-$az);
    return(vector_product(@AB,@AC));
}

################################################################################

sub
open_from_file
{
    my ($self, $file) = @_;

#    $file ||= $main_win->getOpenFile(
#        -initialdir => ".", -defaultextension => '.yml', -filetypes => [[ 'Build YML files', [ '.yml' ] ]]);

    return unless $file;

    my $load_data = LoadFile($file);
    $log->debug("load_data ref " . ref $load_data);

    if (ref $load_data eq 'HASH') {
        $self->things($load_data->{things});

        # load the view settings into the model for the app to use as required

        # the ortho hash contains hashes for each ortho view with the settings for that view
        $self->view_setting->{ortho} = $load_data->{views};

        # simple perspective settings; the double perspective below is because we're
        # setting items for the perspective view, and these particular items control
        # the perspective transformation itself, eg close, far, angle.
        $self->view_setting->{perspective}->{perspective} = $load_data->{gl}->{perspective};

        # how the various views (ortho/perspective) will use these vectors is tricky to predict
        # so they're kept separate for now
        for my $vertex (qw(eye target up light)) {
            $self->view_setting->{perspective}->{vertex}->{$vertex} = $load_data->{gl}->{$vertex};
        }

        # flags
        $self->view_setting->{perspective}->{setting} = $load_data->{gl_setting};

    }
    else {
        $log->logdie("no hash from file $file");
    }

    for my $thing (@{ $self->{things} }) {
        if ($thing->{transform_props}) {
            adjust_transform($thing);
        }
        if ($thing->{series_props}) {
            adjust_series($thing);
        }

        delete $thing->{verts};
    }

#    refresh_object_list();

    # HACK
#    map { $_->{listed} = 1; delete $_->{selected}; delete $_->{tk}; } @things;

#    update_listed_thing_indices();
#
#    @vertices = ();
#    $new_vertex = {};
#
#    queue_gl_draw();

    $self->filename($file);
    $log->debug("set filename as $file");

}
 
################################################################################

sub
radiate_vertex_in_plane
{
    my ($vertex, $plane, $angle, $length) = @_;

#    $vertex->[$flat_view{$plane}->{x_dim}] += sin(deg2rad($angle)) * $length;
#    $vertex->[$flat_view{$plane}->{y_dim}] += cos(deg2rad($angle)) * $length;

}

################################################################################
        
sub
find_ring_points 
{
    my ($anchor_vertex, $plane, $angle, $radius, $angle_change, $count) = @_;

    my @points = ();

    # note that the anchor vertex will not appear in the output list (unless the ring
    # goes right around and generates it again)
    my @pos = @{ $anchor_vertex };

    $angle += $angle_change;
    $angle %= 360;
    for my $index (1 .. $count) {
        radiate_vertex_in_plane(\@pos, $plane, $angle, $radius);
        $angle += $angle_change;
        $angle %= 360;
        push @points, [ @pos ];
    }
    return @points;
}
 
################################################################################

sub
copy_thing_to_pos
{
    my ($thing, $vertex) = @_;

    my $new_thing = {
        name => $thing->{name},
        type => $thing->{type},
        color => dclone $thing->{color},
        solid => $thing->{solid},
        listed => 0,
        sizes => dclone $thing->{sizes},
        transform_type => $thing->{transform_type},
        # deliberately not dcloning here, so changes to the master's transform
        # will affect the others
        transform_props => $thing->{transform_props},
        _transform_props => $thing->{_transform_props},
        vert => $vertex,
        do_not_save => 1,
    };

    return $new_thing;
}

################################################################################
 
sub
adjust_series
{
    my $thing = shift @_;
    my $log = get_logger();

    $log->debug("adjust_series $thing");

    # we'll replace any existing series objects for this thing (don't worry about the object listbox,
    # we'll refresh the whole thing)
    $thing->{_series_things} ||= [];
    my $existing_series_size = scalar @{ $thing->{_series_things} };

    # series things follow their owner so we just need to find the owner in the list
    my $index = (firstidx { $_ == $thing } @things) + 1;

    my $props = $thing->{series_props}->{$thing->{series_type}};

    # build the new things
    my @new_things = ();
    my @pos = @{ $thing->{vert} };
    if ($thing->{series_type} eq 'line') {
        for my $index (1 .. $props->{Entries}) {
            $pos[0] += $props->{XInc};
            $pos[1] += $props->{YInc};
            $pos[2] += $props->{ZInc};
            my $new_thing = copy_thing_to_pos($thing, [ @pos ]);
            push @new_things, $new_thing;
        }
    }
    elsif ($thing->{series_type} eq 'arc') {
        my @arc_positions = find_ring_points(
            $thing->{vert},
            $props->{Plane},
            $props->{AngleStart},
            $props->{SideLength},
            $props->{AngleChange},
            $props->{Entries},
            );
        for my $vertex (@arc_positions) {
            my $new_thing = copy_thing_to_pos($thing, $vertex);
            push @new_things, $new_thing;
        }
    }
    elsif ($thing->{series_type} eq 'cloud') {
        my @conditions = ();

        my ($x_min,$x_max) = $props->{XCentred}
            ? (-$props->{XEntries}/2, $props->{XEntries}/2)
            : (0, $props->{XEntries}-1);
        my ($y_min,$y_max) = $props->{YCentred}
            ? (-$props->{YEntries}/2, $props->{YEntries}/2)
            : (0, $props->{YEntries}-1);
        my ($z_min,$z_max) = $props->{ZCentred}
            ? (-$props->{ZEntries}/2, $props->{ZEntries}/2)
            : (0, $props->{ZEntries}-1);

        for my $condition (split(/\n/ms, $props->{Conditions})) {
            my $flags = '';
            if ($condition =~ /(.+):(.+)/) {
                ($condition,$flags) = ($1,$2); 
            }
            $condition =~ s/x/(\$x_index)/g;
            $condition =~ s/y/(\$y_index)/g;
            $condition =~ s/z/(\$z_index)/g;
            $condition = "\$flag = ($condition)";
            $log->debug("condition: $condition");
            push @conditions, $condition;
        }
        for my $x_index ($x_min .. $x_max) {
            for my $y_index ($y_min .. $y_max) {
                for my $z_index ($z_min .. $z_max) {
                    my $flag;
                    for my $condition (@conditions) {
                        eval $condition;
                        last unless $flag;
                    }
                    next unless $flag;
                    $log->debug("$pos[0] $x_index $pos[1] $y_index $pos[2] $z_index");
                    my $vertex = [
                        $pos[0] + $x_index * $props->{XInc},
                        $pos[1] + $y_index * $props->{YInc},
                        $pos[2] + $z_index * $props->{ZInc},
                    ];
                    my $new_thing = copy_thing_to_pos($thing, $vertex);
                    delete $new_thing->{no_render};
                    push @new_things, $new_thing;
                }
            }
        }

        $thing->{no_render} = $props->{HideOriginal};
    }

    # add the things and redraw
    splice @things, $index, $existing_series_size, @new_things;
    $thing->{_series_things} = \@new_things;
#     queue_gl_draw();
#     refresh_object_list();
#     update_listed_thing_indices();

    return;
}

# ################################################################################

sub
adjust_transform
{
    my $thing = shift @_;
    my $log = get_logger();

    $log->debug("adjust_transform $thing $thing->{transform_type}");
    my $props = $thing->{transform_props}->{$thing->{transform_type}};
    my $calc_props = $thing->{_transform_props}->{$thing->{transform_type}} ||= {};

    my @side_points;

    if ($thing->{transform_type} eq 'none') {
        $thing->{type} = $WxBuild::OT_CUBOID;
    }
    elsif ($thing->{transform_type} eq 'prism') {
        # properties:
        # Sides, Plane, Angle, Size, CustomSize, 

        # we need to define the normals and vertices used by draw_prism.
        # Note that the object should be positioned centred around the origin;
        # it will be translated into the correct location on rendering.

        my $radius;
        if ($props->{Size} eq 'Small') {
#            $radius = min($thing->{sizes}->[$flat_view{$props->{Plane}}->{x_dim}], $thing->{sizes}->[$flat_view{$props->{Plane}}->{y_dim}]);
            $radius /= 2;
        }
        elsif ($props->{Size} eq 'Large') {
#            $radius = max($thing->{sizes}->[$flat_view{$props->{Plane}}->{x_dim}], $thing->{sizes}->[$flat_view{$props->{Plane}}->{y_dim}]);
            $radius /= 2;
        }
        elsif ($props->{Size} eq 'Custom') {
            $radius = $props->{CustomSize};
        }
        else {
            $log->logdie("bad size $props->{Size}");
        }

        my $angle_change = 360 / $props->{Sides};

        my $start_angle = 90 - ($angle_change / 2) + $props->{Angle};

        # define the ends
        delete $calc_props->{end_vertices};
        for my $end (0,1) {

            # the centre of the prism is always at 0,0 wrt the plane of the prism
            my $centre_end = [ 0,0,0 ];

            # move to positive or negative in the dimension of the prism
#            $centre_end->[$flat_view{$props->{Plane}}->{other_dim}] += ($end ? 1 : -1) * ($thing->{sizes}->[$flat_view{$props->{Plane}}->{other_dim}] / 2);

            # the centre point is the first point we need for rendering
            push @{ $calc_props->{end_vertices}->[$end] }, $centre_end;

            my $end_radius = $radius * ($end ? $props->{PositiveEndSize} : $props->{NegativeEndSize});

            # move centre to first point on rim
            my $first_point = dclone $centre_end;
            radiate_vertex_in_plane($first_point, $props->{Plane}, $props->{Angle}, $end_radius);

            my $side_length = 2 * $end_radius * sin(deg2rad($angle_change/2));

            # generate points around rim; note that the first point only appears in this list as the final point,
            # since we define the angle change and count to do a full revolution.
            my @rim_points = find_ring_points(
                $first_point,
                $props->{Plane},
                $start_angle,
                $side_length,
                $angle_change,
                $props->{Sides},
                );

            # the original point on the rim is the next rendering point
            unshift @rim_points, $first_point;

            # we now have the complete list of points around the rim; by combining these lists from
            # both ends we get the quad_strip lists for the side faces.
            if (@side_points) {
                # second loop; mesh them together
                $calc_props->{side_points} = [ mesh @rim_points, @side_points ];
            }
            else {
                # first loop, just record them
                @side_points = @rim_points;
            }

            # add the rim points
            push @{ $calc_props->{end_vertices}->[$end] }, @rim_points;

            # we can now use the first 3 points in the vertices list to find the normal to the plane
            my $end_flag = $props->{Plane} eq 'XY' ? ! $end : $end;
            if ($end_flag) {
                $calc_props->{end_normal}->[$end] = [ triangle_normal(
                    @{ $calc_props->{end_vertices}->[$end]->[0] },
                    @{ $calc_props->{end_vertices}->[$end]->[1] },
                    @{ $calc_props->{end_vertices}->[$end]->[2] },
                    ) ];
            }
            else {
                $calc_props->{end_normal}->[$end] = [ triangle_normal(
                    @{ $calc_props->{end_vertices}->[$end]->[0] },
                    @{ $calc_props->{end_vertices}->[$end]->[2] },
                    @{ $calc_props->{end_vertices}->[$end]->[1] },
                    ) ];
            }

        }

        # calculate side normals
        $calc_props->{side_normals} = [];
        for my $index (1 .. scalar @{ $calc_props->{side_points}} / 2 - 1) {
            if ($props->{Plane} eq 'XY') {
                push @{ $calc_props->{side_normals} },
                    [ triangle_normal(
                        @{ $calc_props->{side_points}->[ $index * 2 - 2] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 0] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 1] },
                    )];
            }
            else {
                push @{ $calc_props->{side_normals} },
                    [ triangle_normal(
                        @{ $calc_props->{side_points}->[ $index * 2 - 2] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 1] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 0] },
                    )];
            }
        }

        $log->debug("transform props: " . Dumper($props));
        $log->debug("transform calc props: " . Dumper($calc_props));
        $thing->{type} = $WxBuild::OT_PRISM;

    }
    elsif ($thing->{transform_type} eq 'stairs') {
        # props: Steps, Plane, Rotation

#        my $plane_view = $flat_view{$props->{Plane}};
#
#        # define the ends
#        delete $calc_props->{end_vertices};
#        for my $end (0,1) {
#
#            # anticlockwise for the -ve end
#            my @corners = (
#                [ -1,1 ],
#                [ 1,1 ],
#                [ 1,-1 ],
#                [ -1,-1 ],
#            );
#
#            # we rotate the corners for the end then use the first 3 as the corners for the end triangle
#            for my $i (1 .. $props->{Rotation}) {
#                push @corners, shift @corners;
#            }
#
#            $log->debug("corners = " . Dumper(\@corners));
#
#            for my $i (0 .. 2) {
#                my $vertex = [];
#                $vertex->[$plane_view->{other_dim}] = ($end ? 1 : -1) * ($thing->{sizes}->[$plane_view->{other_dim}] / 2);
#                $vertex->[$plane_view->{x_dim}] = $corners[$i]->[0] * ($thing->{sizes}->[$plane_view->{x_dim}] / 2);
#                $vertex->[$plane_view->{y_dim}] = $corners[$i]->[1] * ($thing->{sizes}->[$plane_view->{y_dim}] / 2);
#                push @{ $calc_props->{end_vertices}->[$end] }, $vertex;
#            }
#
#            # we can now use the first 3 points in the vertices list to find the normal to the end plane
#            my $end_flag = $props->{Plane} eq 'XY' ? ! $end : $end;
#            if ($end_flag) {
#                $calc_props->{end_normal}->[$end] = [ triangle_normal(
#                    @{ $calc_props->{end_vertices}->[$end]->[0] },
#                    @{ $calc_props->{end_vertices}->[$end]->[1] },
#                    @{ $calc_props->{end_vertices}->[$end]->[2] },
#                    ) ];
#            }
#            else {
#                $calc_props->{end_normal}->[$end] = [ triangle_normal(
#                    @{ $calc_props->{end_vertices}->[$end]->[0] },
#                    @{ $calc_props->{end_vertices}->[$end]->[2] },
#                    @{ $calc_props->{end_vertices}->[$end]->[1] },
#                    ) ];
#            }
#
#            if ($props->{Steps} == 1) {
#
#                # Steps == 1 is a ramp; the 3 points is all we need for the ends
#                $thing->{type} = $WxBuild::OT_RAMP;
#
#            }
#            else {
#                $thing->{type} = $WxBuild::OT_STAIRS;
#
#                # Steps > 1 means we need to add steps between the diagonally opposed points,
#                # which are the first and last in the end triangle (since we start with a list of
#                # 4 points going around the square and then leave one off the end to find the triangle).
#                my $from = $calc_props->{end_vertices}->[$end]->[2];
#                my $to = $calc_props->{end_vertices}->[$end]->[0];
#                my $x_inc = ($to->[$plane_view->{x_dim}] - $from->[$plane_view->{x_dim}]) / $props->{Steps};
#                my $y_inc = ($to->[$plane_view->{y_dim}] - $from->[$plane_view->{y_dim}]) / $props->{Steps};
#                for my $i (1 .. $props->{Steps}) {
#
#                    # each step is a triangle joined onto the ramp triangle
#
#                    my $vertex = dclone $from;
#                    $vertex->[$plane_view->{x_dim}] += $x_inc * ($i - 1);
#                    $vertex->[$plane_view->{y_dim}] += $y_inc * ($i - 1);
#                    push @{ $calc_props->{end_vertices}->[$end] }, $vertex;
#
#                    $vertex = dclone $from;
#                    if ($props->{Rotation} % 2) {
#                        $vertex->[$plane_view->{x_dim}] += $x_inc * ($i - 1);
#                        $vertex->[$plane_view->{y_dim}] += $y_inc * $i;
#                    }
#                    else {
#                        $vertex->[$plane_view->{x_dim}] += $x_inc * $i;
#                        $vertex->[$plane_view->{y_dim}] += $y_inc * ($i - 1);
#                    }
#                    push @{ $calc_props->{end_vertices}->[$end] }, $vertex;
#
#                    $vertex = dclone $from;
#                    $vertex->[$plane_view->{x_dim}] += $x_inc * $i;
#                    $vertex->[$plane_view->{y_dim}] += $y_inc * $i;
#                    push @{ $calc_props->{end_vertices}->[$end] }, $vertex;
#
#                }
#            }
#
#            # For the purpose of drawing the sides, the rim points have the start point at the end as well.
#            # To do the mesh thing, we have to have a real array variable, we can't use a () construct
#            my @rim_points = ( @{ $calc_props->{end_vertices}->[$end] },
#                $calc_props->{end_vertices}->[$end]->[0]);
#
#            # now we have the full end point lists, we can transform these lists into the side lists
#            if (@side_points) {
#                # second loop; mesh them together
#                $calc_props->{side_points} = [ mesh @rim_points, @side_points ];
#            }
#            else {
#                # first loop, just record them
#                @side_points = @rim_points;
#            }
#
#        }
#
#        # calculate side normals
#        $calc_props->{side_normals} = [];
#        for my $index (1 .. scalar @{ $calc_props->{side_points}} / 2 - 1) {
#            if ($props->{Plane} eq 'XY') {
#                push @{ $calc_props->{side_normals} },
#                    [ triangle_normal(
#                        @{ $calc_props->{side_points}->[ $index * 2 - 2] },
#                        @{ $calc_props->{side_points}->[ $index * 2 - 0] },
#                        @{ $calc_props->{side_points}->[ $index * 2 - 1] },
#                    )];
#            }
#            else {
#                push @{ $calc_props->{side_normals} },
#                    [ triangle_normal(
#                        @{ $calc_props->{side_points}->[ $index * 2 - 2] },
#                        @{ $calc_props->{side_points}->[ $index * 2 - 1] },
#                        @{ $calc_props->{side_points}->[ $index * 2 - 0] },
#                    )];
#            }
#        }
#
#        $log->debug("transform props: " . Dumper($props));
#        $log->debug("transform calc props: " . Dumper($calc_props));
    }

#    queue_gl_draw();
    return;
}
 
# ################################################################################
# 
# undo_last_click
# {
#     # undo button; back up a step
#     if (scalar @vertices) {
#         if ($new_vertex->{from_view}) {
#             $log->debug("undo c");
#             $new_vertex = {};
#             for my $other_view (values %flat_view) {
#                 $other_view->{canvas}->coords($other_view->{selected_line_x}, 0,0,0,0);
#                 $other_view->{canvas}->coords($other_view->{selected_line_y}, 0,0,0,0);
#             }
#             if ($new_thing) {
#                 $new_thing = undef;
#                 $object_list->delete('end');
#                 pop @things;
#                 pop @listed_thing_indices;
#             }
#         }
#         else {
#             $log->debug("undo b");
#             # the top vertex on @vertices should become $new_vertex instead, and we should
#             # clear the last dimension set, which is the dimension undefined for the view
#             # that created the vertex
#             $new_vertex = pop @vertices;
#             if ($new_vertex->{from_view} eq 'XZ') {
#                 $new_vertex->{vertex}->[1] = undef;
#             }
#             elsif ($new_vertex->{from_view} eq 'YZ') {
#                 $new_vertex->{vertex}->[0] = undef;
#             }
#             elsif ($new_vertex->{from_view} eq 'XY') {
#                 $new_vertex->{vertex}->[2] = undef;
#             }
#             else {
#                 die;
#             }
#             for my $other_view (values %flat_view) {
#                 $other_view->{canvas}->coords($other_view->{new_object_indicator}, 0,0,0,0);
#             }
#             for my $other_view (values %flat_view) {
# 
#                 my @view_coords = model_xyz_to_view_xy($other_view, @{ $new_vertex->{vertex} });
# 
#                 $other_view->{canvas}->coords($other_view->{selected_line_x}, $view_coords[0], 0, $view_coords[0], $view_height);
#                 $other_view->{canvas}->coords($other_view->{selected_line_y}, 0, $view_coords[1], $view_width, $view_coords[1]);
#             }
#         }
#     }
#     else {
#         if ($new_vertex->{from_view}) {
#             $log->debug("undo a");
#             $new_vertex = {};
#             for my $other_view (values %flat_view) {
#                 $other_view->{canvas}->coords($other_view->{selected_line_x}, 0,0,0,0);
#                 $other_view->{canvas}->coords($other_view->{selected_line_y}, 0,0,0,0);
#             }
#         }
#     }
#     queue_gl_draw();
#     return;
# }
# 
# ################################################################################
# 
# sub
# thing_passes_filter
# {
#     my $thing = shift;
# 
#     return 1 unless $thing->{vert};
# 
#     my $show = 1;
#     for my $dim (0 .. 2) {
#         my $max = $thing->{vert}->[$dim];
#         $max += $thing->{sizes}->[$dim] if $show_filter_includes_dimension;
#         $show = $filter_by_intersection
#             ? ($thing->{vert}->[$dim] <= $show_filter_pos_max[$dim]
#                 || $max <= $show_filter_pos_min[$dim])
#             : ($thing->{vert}->[$dim] >= $show_filter_pos_min[$dim]
#                 && $max <= $show_filter_pos_max[$dim]);
#         last unless $show;
#     }
# 
#     return $show;
# }

################################################################################

1;
