use strict;
package WxBuildView;
use Moose;
use Log::Log4perl qw(get_logger);

use Data::Dumper;
use YAML qw(LoadFile DumpFile);
use List::Util qw(min max);
use List::MoreUtils qw(firstidx mesh);
use Storable qw(dclone);
use Math::Trig;

use Wx qw(wxTheApp);

use WxBuild;
use WxBuildVertex;

my $log = get_logger();

has 'model' => ( 
    isa => 'WxBuildModel', 
    is => 'rw',
#    required => 1,
    weak_ref => 1,
);

has 'panel' => ( 
    isa => 'WxBuildViewPanel', 
    is => 'ro',
    required => 1,
    weak_ref => 1,
);

has 'canvas' => ( 
    isa => 'WxBuildCanvas', 
    is => 'rw',
    weak_ref => 1,
);

has 'name' => ( 
    isa => 'Str', 
    is => 'rw',
);

has 'cursor_pos' => (
    isa => 'ArrayRef', 
    is => 'rw',
    default => sub { [10,10,10] },
);

has 'size' => ( 
    isa => 'Num', 
    is => 'rw',
    default => 100,
);

has 'size_factor' => ( 
    isa => 'Num', 
    is => 'rw',
    default => 0,
);

has 'x_centre' => ( 
    isa => 'Num', 
    is => 'rw',
    default => 0,
);

has 'y_centre' => ( 
    isa => 'Num', 
    is => 'rw',
    default => 0,
);

has 'model_left_side' => ( 
    isa => 'Num', 
    is => 'rw',
);

has 'model_top_side' => ( 
    isa => 'Num', 
    is => 'rw',
);

has 'x_dim' => ( 
    isa => 'Int', 
    is => 'rw',
);

has 'y_dim' => ( 
    isa => 'Int', 
    is => 'rw',
);

has 'x_axis_direction' => ( 
    isa => 'Int', 
    is => 'rw',
);

has 'y_axis_direction' => ( 
    isa => 'Int', 
    is => 'rw',
);

has 'precision' => ( 
    isa => 'Int', 
    is => 'rw',
    default => 2,
);

has 'other_dim' => ( 
    isa => 'Int', 
    is => 'rw',
);

has 'perspective' => (
    isa => 'HashRef',
    is => 'ro',
    default => sub { { angle => 60, near => 1, far => 1000, } },
);

has 'scale' => (
    isa => 'WxBuildVertex',
    is => 'ro',
    default => sub { WxBuildVertex->new(x => 1, y => 1, z => 1, inc => 0.5), },
);

has 'eye' => (
    isa => 'WxBuildVertex',
    is => 'rw',
    default => sub { WxBuildVertex->new(x => 20, y => 10, z => 40, inc => 0.5), },
);

has 'light' => (
    isa => 'WxBuildVertex',
    is => 'ro',
    default => sub { WxBuildVertex->new(x => 20, y => 40, z => 40, inc => 1), },
);

has 'target' => (
    isa => 'WxBuildVertex',
    is => 'ro',
    default => sub { WxBuildVertex->new(x => 0, y => 0, z => 0, inc => 0.5), },
);

has 'up' => (
    isa => 'WxBuildVertex',
    is => 'ro',
    default => sub { WxBuildVertex->new(x => 20, y => 20, z => 40, inc => 0.5), },
);

has 'up_vector_above_eye' => (
    isa => 'Bool',
    is => 'rw',
    default => 1,
);

has 'eye_follows_cursor' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

has 'light_follows_cursor' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

has 'target_follows_cursor' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

has 'render_by_selection' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

has 'render_unselected' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

has 'filter_shown_by_position' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

has 'show_filter_includes_dimension' => (
    isa => 'Bool',
    is => 'rw',
    default => 1,
);

has 'filter_by_intersection' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

has 'show_filter_pos_min' => (
    isa => 'WxBuildVertex',
    is => 'ro',
    default => sub { WxBuildVertex->new(x => -100, y => -100, z => -100, inc => 5), },
);

has 'show_filter_pos_max' => (
    isa => 'WxBuildVertex',
    is => 'ro',
    default => sub { WxBuildVertex->new(x => 100, y => 100, z => 100, inc => 5), },
);

# various option/arg flags

has 'debug_flag' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

# divisions for point selection
has 'divisions' => ( 
    isa => 'Int', 
    is => 'rw',
    default => 4,
);

# snap x
has 'snap_x' => ( 
    isa => 'Num', 
    is => 'rw',
);

# snap y
has 'snap_y' => ( 
    isa => 'Num', 
    is => 'rw',
);

# snap radius
has 'snap_radius' => ( 
    isa => 'Int', 
    is => 'rw',
    default => 5,
);

# prev_drag x
has 'prev_drag_x' => ( 
    isa => 'Num', 
    is => 'rw',
);

# prev_drag y
has 'prev_drag_y' => ( 
    isa => 'Num', 
    is => 'rw',
);

# dragging flag
has 'dragging' => ( 
    isa => 'Bool', 
    is => 'rw',
    default => 0,
);

# ortho view flag
has 'ortho' => ( 
    isa => 'Bool', 
    is => 'rw',
    default => 0,
);

# the location of the eye position along the axis not visible in this
# view is largely a matter of convention.
has 'eye_direction' => ( 
    isa => 'Num', 
    is => 'ro',
);

has 'controls_shown' => ( 
    # track the display of the controls so we know whether to update them
    isa => 'Bool', 
    is => 'rw',
    default => 0,
);

after 'size' => sub {
    my ($self, $size) = @_;
    $self->recalc_view if defined $size && $self->ortho;
};

after 'x_centre' => sub {
    my ($self, $x_centre) = @_;
    $self->recalc_view if defined $x_centre && $self->ortho;
};

after 'y_centre' => sub {
    my ($self, $y_centre) = @_;
    $self->recalc_view if defined $y_centre && $self->ortho;
};

after 'model' => sub {
    my ($self, $model) = @_;
    if ($model) {
        $log->debug("setting model for view " . $self->name);
        $self->canvas->model($model);
    }
};

__PACKAGE__->meta->make_immutable;

sub BUILD {
    my ($self, $args) = @_;

#    $log->debug("build: " . Dumper($args));
#    $log->debug("build: " . Dumper($self));

    # canvas & view are permanent for the app's life, so set these once only
    $self->canvas($self->panel->{canvas});

#    $self->canvas->model($self->model);

    if (defined($self->{x_dim})) {
        $self->canvas->ortho(1);
        $self->ortho(1);

        $self->eye($self->default_eye_vertex);

        $self->panel->{eye_x_spc}->SetValue($self->eye->x);
        $self->panel->{eye_y_spc}->SetValue($self->eye->y);
        $self->panel->{eye_z_spc}->SetValue($self->eye->z);

    }
    else {

        $self->panel->{perspective_angle_spc}->SetValue($self->perspective->{angle});
        $self->panel->{perspective_near_spc}->SetValue($self->perspective->{near});
        $self->panel->{perspective_far_spc}->SetValue($self->perspective->{far});
    }

    # make sure we've set ortho before this
    $self->canvas->view($self);

    $self->canvas->InitGL;

}

################################################################################

sub 
debug
{
    my $self = shift;

    if ($self->debug_flag) {
        $log->debug(@_);
    }
}

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

sub toggle_controls {
    my ($self) = @_;

    $self->controls_shown(!$self->controls_shown);
    $self->panel->toggle_controls;
    $self->recalc_view;
    $self->refresh;
}

################################################################################

sub reset_eye {
    my ($self) = @_;

    $self->eye($self->default_eye_vertex);
    $self->refresh;

    return;
}

################################################################################

sub reset_view {
    my ($self) = @_;

    $self->x_centre(0);
    $self->y_centre(0);
    $self->size(40);
    $self->recalc_view;
    $self->refresh;

    return;
}

################################################################################

sub refresh {
    my ($self) = @_;

    $self->canvas->dirty(1);

    if ($self->controls_shown) {
        $self->panel->{eye_x_spc}->SetValue($self->eye->x);
        $self->panel->{eye_y_spc}->SetValue($self->eye->y);
        $self->panel->{eye_z_spc}->SetValue($self->eye->z);
    }

    return;
}

################################################################################

sub default_eye_vertex {
    my ($self) = @_;

    my $vertex = WxBuildVertex->new();

    # the eye lies along the other axis, the direction is set only from
    # what is the familiar orientation.
    $vertex->dim($self->other_dim, 100 * $self->eye_direction);

    return $vertex;
}

################################################################################

sub in_2d_view {
    my ($self) = @_;

#    return $self->eye->equals($self->default_eye_vertex);
    return 1;
}

################################################################################

sub zoom_tracker {
    my ($canvas, $event) = @_;

    my $inc = $event->GetWheelRotation > 0 ? -1 : 1;
    my $self = $canvas->view;

    if ($self->ortho) {

        # scroll wheel adjusts size for ortho views
        if ($event->ControlDown) {
            $log->debug("ctrl");
            $inc *= 4;
        }

        $self->size($self->size + ($self->size / 5) * $inc);
    }
    else {

        # scroll wheel moves eye position closer/further from target in perspective view
        for my $dim (0 .. 2) {
            $self->eye->dim($dim, $self->eye->dim($dim) + ($self->eye->dim($dim) - $self->target->dim($dim)) / 5 * $inc);
        }
    }

    $self->refresh;

}

################################################################################

sub 
drag_tracker {
# click-movement sub
    my ($self, $x, $y) = @_;

    # we can't calculate model position from view position unless we're looking along an axis
    return unless $self->in_2d_view;

    $log->debug("+drag_tracker+ at $x,$y\n");
    
    $self->dragging(1);

    # record the position as the snap position so the click handler gets the right point
    $self->snap_x($x);
    $self->snap_y($y);

#    if ($drag_to_show_flag) {
#
#        # adjust the show filter coordinates controlled by this view
#        my ($min_x, $max_x) = sort numeric ($x, $self->prev_drag_x);
#        my ($min_y, $max_y) = sort rev_numeric ($y, $self->prev_drag_y);
#
#        my @coord = view_xy_to_model_xyz($view, $min_x, $min_y);
#        for my $dim (0,1,2) {
#            $show_filter_pos_min[$dim] = $coord[$dim] if defined $coord[$dim];
#        }
#        @coord = view_xy_to_model_xyz($view, $max_x, $max_y);
#        for my $dim (0,1,2) {
#            $show_filter_pos_max[$dim] = $coord[$dim] if defined $coord[$dim];
#        }
#        $filter_shown_by_position = 1;
#    }
#    else {

        # adjust the views according to the displacement from the last drag point
        my $delta_x = $x - $self->prev_drag_x;
        my $delta_y = $y - $self->prev_drag_y;
        $log->debug("drag delta = $delta_x, $delta_y");

        # convert to model coords; this is not a true transformation of canvas coords (since all
        # we have are offsets) so we can't use view_xy_to_model_xyz().
        $delta_x = $delta_x / $self->size_factor;
        $delta_y = $delta_y / $self->size_factor;

        $self->x_centre($self->x_centre - ($self->x_axis_direction * $delta_x));
        $self->y_centre($self->y_centre - ($self->y_axis_direction * $delta_y));

        $self->prev_drag_x($x);
        $self->prev_drag_y($y);

#    }

    $self->recalc_view;
    $self->refresh;

#    # the refresh deletes all indicators so do this now
#    if ($drag_to_show_flag) {
#
#        # we never update prev_drag_* so we can draw the rectangle to there every time
#        $view->{canvas}->coords($view->{new_object_indicator}, $self->prev_drag_x,$self->prev_drag_y, $x,$y);
#    }

}

################################################################################

sub move_tracker {
    my ($self, $x, $y, $flags) = @_;
    my $app = wxTheApp->{app};

    $app->current_view($self);

    # we can't calculate model position from view position unless we're looking along an axis
    return unless $self->in_2d_view;

    my $do_gl_draw = 0;

    # ignore early clicks before size factor calced
    return unless $self->size_factor;

#    $log->debug(sprintf "move_tracker: [@_] %s %d %d %d", $self->name, $x, $y, $self->size_factor);

    my @model_coords = $self->view_xy_to_model_xyz($x, $y);
#    $log->debug("model_coords = " . Dumper(\@model_coords));

    my $cursor_line = $app->system_thing->{"cursor_line_" . $self->x_dim};
    $cursor_line->{verts}->[0]->[$self->y_dim] = $model_coords[$self->y_dim];
    $cursor_line->{verts}->[1]->[$self->y_dim] = $model_coords[$self->y_dim];
#    $log->debug("cursor line for " . $self->x_dim . Dumper($cursor_line->{verts}));

    $cursor_line = $app->system_thing->{"cursor_line_" . $self->y_dim};
    $cursor_line->{verts}->[0]->[$self->x_dim] = $model_coords[$self->x_dim];
    $cursor_line->{verts}->[1]->[$self->x_dim] = $model_coords[$self->x_dim];
#    $log->debug("cursor line for " . $self->y_dim . Dumper($cursor_line->{verts}));

    $cursor_line = $app->system_thing->{"cursor_line_" . $self->other_dim};
    $cursor_line->{verts}->[0]->[$self->x_dim] = $model_coords[$self->x_dim];
    $cursor_line->{verts}->[1]->[$self->x_dim] = $model_coords[$self->x_dim];
    $cursor_line->{verts}->[0]->[$self->y_dim] = $model_coords[$self->y_dim];
    $cursor_line->{verts}->[1]->[$self->y_dim] = $model_coords[$self->y_dim];
#    $log->debug("cursor line for " . $self->other_dim . Dumper($cursor_line->{verts}));

#    for my $i (0 .. 2) {
#
#        my $cursor_line = $app->system_thing->{"cursor_line_$i"};
#
#        # span the views in this dimension
#        $cursor_line->{verts}->[0]->[$i] = -100;
#        $cursor_line->{verts}->[1]->[$i] = 100;
#
#        # use the defined variable or a substitute (0 or infinity)
#        for my $inc (1,2) {
#            my $other_dim = ($i + $inc) % 3;
#            if (defined $model_coords[$other_dim]) {
#                $cursor_line->{verts}->[0]->[$other_dim] = $model_coords[$other_dim];
#                $cursor_line->{verts}->[1]->[$other_dim] = $model_coords[$other_dim];
#            }
#            else {
#                $cursor_line->{verts}->[0]->[$other_dim] = $self->eye_direction * 1000;
#                $cursor_line->{verts}->[1]->[$other_dim] = $self->eye_direction * 1000;
#            }
#        }
#
#    }

    $app->move_cursor(\@model_coords);

#    unless ($flags->{no_snap}) {
#        # check snaps to see if we should warp the selection lines onto a snap
#        for my $snap ( @{ $self->{snaps} } ) {
#            if ($x > $snap->[0] - $snap_radius && $x < $snap->[0] + $snap_radius) {
#                $x = $snap->[0];
#            }
#            if ( $y > $snap->[1] - $snap_radius && $y < $snap->[1] + $snap_radius) {
#                $y = $snap->[1];
#            }
#        }
#    }

    # record the snapped position; we'll use this instead of the mouse position if a click is received
    $self->snap_x($x);
    $self->snap_y($y);

#    # recalc model coords so all views show the snap; last snap wins
#    @model_coords = view_xy_to_model_xyz($self, $x, $y);
#
#    # calculate lines for all views from model coords since their scales and offsets may be different
#    for my $other_view (values %flat_view) {
#
#        my @view_coords = model_xyz_to_view_xy($other_view, @model_coords);
#
#        # we don't display the selection line if actions in another view have already defined this dimension
#        if ( $new_vertex->{from_view} && $new_vertex->{from_view} ne $self->{name} && defined $new_vertex->{vertex}->[$other_view->{x_dim}] ) {
#            $other_view->{canvas}->coords($other_view->{selection_line_x}, 0,0,0,0);
#        }
#        else {
#            $other_view->{canvas}->coords($other_view->{selection_line_x}, $view_coords[0], 0, $view_coords[0], $view_height)
#        }
#
#        # 
#        if ( $new_vertex->{from_view} && $new_vertex->{from_view} ne $self->{name} && defined $new_vertex->{vertex}->[$other_view->{y_dim}] ) {
#            $other_view->{canvas}->coords($other_view->{selection_line_y}, 0,0,0,0);
#        }
#        else {
#            $other_view->{canvas}->coords($other_view->{selection_line_y}, 0, $view_coords[1], $view_width, $view_coords[1])
#        }
#
#        if ($#vertices > -1) {
#            my $previous_vertex = $vertices[$#vertices];
#            my @previous_view_coords = model_xyz_to_view_xy($other_view, @{ $previous_vertex->{vertex} } );
#            if ($new_vertex->{vertex}) {
#                # we have a previous vertex and an in-progress (ie incomplete by 1 dim) vertex
#                if ($self->{name} eq $new_vertex->{from_view}) {
#                    # if we're on the view that originated the in-progress, we only have 2 dimensions
#                    # so we display only on that view
#                    if ($other_view->{name} eq $new_vertex->{from_view}) {
#                        $other_view->{canvas}->coords($other_view->{new_object_indicator}, @previous_view_coords, @view_coords);
#                    }
#                    else {
#                        $other_view->{canvas}->coords($other_view->{new_object_indicator}, 0,0,0,0);
#                    }
#                }
#                else {
#                    # we've changed views, so combination of in-progress vertex and current position defines a 3d-point,
#                    # so we have a potential final shape to display on all views.
#
#                    $log->debug("new vertex = " . Dumper($new_vertex));
#
#                    # combine in-progress vertex and current location
#                    my @point = @{ $new_vertex->{vertex} };
#                    for my $i (0 .. 2) {
#                        if (defined($model_coords[$i]) && ! defined($point[$i])) {
#                            $point[$i] = $model_coords[$i];
#                        }
#                    }
#
#                    # display box
#                    my @point_coords = model_xyz_to_view_xy($other_view, @point );
#                    $other_view->{canvas}->coords($other_view->{new_object_indicator}, @previous_view_coords, @point_coords);
#
#                    # display a wire-frame version in opengl during final point selection
#                    unless ($new_thing) {
#                        $new_thing = {
#                            name => 'new cuboid',
#                            type => $WxBuild::OT_CUBOID,
#                            color => [ 1, 0, 0 ],
#                            solid => 0,
#                            listed => 1,
#                        };
#                        push @things, $new_thing;
#                        push @listed_thing_indices, $#things;
#                        $object_list->insert('end', $new_thing->{name});
#                    }
#                    my ($p1,$p2) = (\@point, $previous_vertex->{vertex});
#                    for my $i (0 .. 2) {
#                        my ($min, $max) = sort { $a <=> $b } ($p1->[$i], $p2->[$i]);
#                        $new_thing->{vert}->[$i] = $min;
#                        $new_thing->{sizes}->[$i] = $max - $min;
#                    }
#                    $do_gl_draw = 1;
#
#                }
#            }
#            else {
#                if ($other_view == $self) {
#                    $other_view->{canvas}->coords($other_view->{new_object_indicator}, @previous_view_coords, @view_coords);
#                }
#                else {
#                    $other_view->{canvas}->coords($other_view->{new_object_indicator}, 0,0,0,0);
#                }
#            }
#        }
#    }
#
#    $self->refresh();

}

################################################################################

sub click_tracker {
    my ($self, $release, $x, $y) = @_;

    # we can't calculate model position from view position unless we're looking along an axis
    return unless $self->in_2d_view;

    # use the latest snap position instead of the cursor position; no snap
    # means we've clicked before moving, eg an obscuring window has been removed AND
    # we've never moved over the view before. Ignore it to prevent undef warnings.
    return unless defined($self->snap_x) && defined($self->snap_y);

    ($x,$y) = ($self->snap_x, $self->snap_y);

    $log->debug("+click_tracker+ release $release on $self->{name} at $x,$y\n");

    if ($release) {
        $log->debug("release, press at " . $self->prev_drag_x . ',' . $self->prev_drag_y);
#        if ($drag_to_show_flag) {
#            $view->{canvas}->coords($view->{new_object_indicator}, 0,0,0,0);
#            $drag_to_show_flag = 0;
#            return;
#        }
#        else {
            if ($self->dragging) {
                # this is the finish of a drag event, ie button-up; no processing required as yet
#                ($self->prev_drag_x, $self->prev_drag_y) = (undef, undef);
                return;
            }
#        }
    }
    else {
        # record the first drag start
        $self->prev_drag_x($x);
        $self->prev_drag_y($y);
        $self->dragging(0);
        return;
    }

    my @model_coords = $self->view_xy_to_model_xyz($x, $y);

    # scenarios (working in XY as an example)
    # click in XY defines x & y
    # further clicks in XY will redefine x & y in current point
    # selection lines displayed as normal in XY
    # next click in YZ or XZ will set z only
    # selection line for non-z dimensions not displayed in YZ & XZ

    my $app = wxTheApp->{app};
    my $new_vertex = $app->new_vertex;
    my $new_vertices = $app->new_vertices;

    # only 2 model coords will ever be set in @model_coords, so leave the other unchanged
    my $complete = 1;
    for my $i (0 .. 2) {

        my $vertex_line = $app->system_thing->{"vertex_line_$i"};

        if (! $new_vertex->{from_view} || $new_vertex->{from_view} eq $self->name) {
            # subsequent clicks in the same view just reset the initial 2 dims
            $new_vertex->{vertex}->[$i] = $model_coords[$i];
            $new_vertex->{from_view} = $self->name;
            $complete = 0;

            # span the views in this dimension
            $vertex_line->{verts}->[0]->[$i] = -100;
            $vertex_line->{verts}->[1]->[$i] = 100;

            # use the defined variable or a substitute (0 or infinity)
            for my $inc (1,2) {
                my $other_dim = ($i + $inc) % 3;
                if (defined $model_coords[$other_dim]) {
                    $vertex_line->{verts}->[0]->[$other_dim] = $model_coords[$other_dim];
                    $vertex_line->{verts}->[1]->[$other_dim] = $model_coords[$other_dim];
                }
                else {
                    $vertex_line->{verts}->[0]->[$other_dim] = $self->eye_direction * 1000;
                    $vertex_line->{verts}->[1]->[$other_dim] = $self->eye_direction * 1000;
                }
            }

        }
        else {
            $new_vertex->{vertex}->[$i] = $model_coords[$i] unless defined $new_vertex->{vertex}->[$i];
            $complete = 1;
        }

        # we display vertex lines for incomplete vertexes
        $vertex_line->{no_render} = $complete;

        # stages of object creation;
        #
        # Click     View
        #
        # 1         Any         Defines 2 dims of new_vertex.
        #                       Given that we will portray a complete vertex as the intersection
        #                       of three lines, at this stage we cannot display all three since
        #                       one coord is undef. In fact, the only line we can complete is
        #                       the line parallel to the axis which is invisible in the current view,
        #                       since the third coord for that line is automatically set to the maximum
        #                       view extent in that dimension (so the line spans all views).
        #                       For the other 2 lines, we have only 2 useful dimensions; the one we clicked,
        #                       and the view-spanning one. For the third we can either use 0 
        #                       or an arbitrarily large value to move the lines out of view.

#        $selected_statusvar[$i] = $new_vertex->{vertex}->[$i] || '';
    }

    if ($complete) {

        push @{ $new_vertices }, $new_vertex;
        $log->debug("finish vertex " . Dumper($new_vertex));
        if ($#{ $new_vertices } % 2) {

            # remove the new vertices from the list
            my ($v1,$v2) = splice @{ $new_vertices }, 0, 2;

            # new_thing is already in list, so just update the location and make it solid
            my $new_thing = $app->new_thing;
            $new_thing->{solid} = 1;
            for my $i (0 .. 2) {
                my ($min, $max) = sort { $a <=> $b } ($v1->{vertex}->[$i], $v2->{vertex}->[$i]);
                $new_thing->{vert}->[$i] = $min;
                $new_thing->{sizes}->[$i] = $max - $min;
            }
#            $object_list->selectionSet('end');
#            # force selection event
#            object_list_select_handler($object_list);

            # this leaves the new_thing in the list, just clear the working version so we can create new ones
            $app->new_thing(undef);

#            queue_gl_draw();
        }
        $app->new_vertex({});

    }
    else {

        $log->debug("start vertex");

    }

    $app->refresh_all_views();

}

# ################################################################################
# 
# sub
# change_view_snap
# {
#     my ($view, $x_inc, $y_inc) = @_;
#     $log->debug("view $view, x_inc $x_inc, y_inc $y_inc");
# 
#     # convert the current snap settings back to model coords, adjust the model coords by
#     # the specified amount, and convert back to view coords
#     my @model_snap = view_xy_to_model_xyz($view, $self->snap_x, $self->snap_y);
# 
#     $model_snap[ $view->{x_dim} ] += $x_inc;
#     $model_snap[ $view->{y_dim} ] += $y_inc * $view->{y_axis_direction};
# 
#     ($self->snap_x, $self->snap_y) = model_xyz_to_view_xy($view, @model_snap);
# 
#     move_tracker($current_view->{canvas}, $current_self->snap_x, $current_self->snap_y, { no_snap => 1 } );
# }
# 
################################################################################

sub
recalc_view
{
    my ($self) = @_;

    unless ($self->ortho) {
        $log->logdie("called from " . join("\n", caller));
    }

    my ($view_width, $view_height) = $self->canvas->GetSizeWH;
    $self->debug("recalc_view: $view_width, $view_height");

    my $aspect_ratio = $view_width / $view_height;

#        $view->{canvas}->configure(-background => $view->{bg} );
#        $view->{canvas}->createText(10, 10, -anchor => 'nw', -text => $view->{name});

    # work out zoom
    if ($view_width > $view_height) {

        $self->model_left_side( $self->x_centre - $self->size * $aspect_ratio / 2 * $self->x_axis_direction);
        $self->model_top_side( $self->y_centre - $self->size / 2 * $self->y_axis_direction );
        $self->size_factor( $view_height / $self->size);
    }
    else {
        $self->model_left_side( $self->x_centre - $self->size / 2 * $self->x_axis_direction);
        $self->model_top_side( $self->y_centre - $self->size / $aspect_ratio / 2 * $self->y_axis_direction);
        $self->size_factor( $view_width / $self->size);
    }

    $self->debug(sprintf "left %d, top %d, size %d, size_factor %f", $self->model_left_side, $self->model_top_side, $self->size, $self->size_factor);

}

################################################################################

sub
view_xy_to_model_xyz
{
    my ($self, $canvas_x, $canvas_y) = @_;
    $self->debug("view_xy_to_model_xyz: x, y = $canvas_x, $canvas_y");

    confess "no size factor" unless $self->size_factor;

    my @model_coords;

    my ($view_width, $view_height) = $self->canvas->GetSizeWH;

    $model_coords[ $self->x_dim ] = sprintf '%.' . $self->precision . 'f', 
        $self->model_left_side + $canvas_x / $self->size_factor * $self->x_axis_direction;
#    $self->debug(sprintf "x_dim = %s, left = %.2f, x = %d, width = %d, sf = %.2f", 
#        $model_coords[ $self->x_dim ], $self->model_left_side, $canvas_x, $view_width, $self->size_factor);
    $model_coords[ $self->y_dim ] = sprintf '%.' . $self->precision . 'f', 
        $self->model_top_side + $canvas_y / $self->size_factor * $self->y_axis_direction;
#    $self->debug(sprintf "y_dim = %s, left = %.2f, y = %d, width = %d, sf = %.2f", 
#        $model_coords[ $self->y_dim ], $self->model_left_side, $canvas_y, $view_height, $self->size_factor);

    return @model_coords;
}

################################################################################
# 
# sub
# model_xyz_to_view_xy
# {
#     my ($view, @model_coords) = @_;
# #    $log->debug("model_xyz_to_view_xy: $view->{name}, model_coords = @model_coords");
# 
#     my $view_x = defined($model_coords[ $view->{x_dim} ])
#         ? $view_width - ((($model_coords[ $view->{x_dim} ] * $view->{x_axis_direction}) - $view->{model_left_side}) * $self->size_factor )
#         : 0;
#     my $view_y = defined($model_coords[ $view->{y_dim} ])
#         ? $view_height - ((($model_coords[ $view->{y_dim} ] * $view->{y_axis_direction}) - $view->{model_top_side}) * $self->size_factor )
#         : 0;
# 
#     return ($view_x, $view_y);
# }
# 
# ################################################################################
# 
# sub
# add_snap_target
# {
#     my ($view, $x, $y) = @_;
#     $view->{canvas}->createOval($x - $snap_radius, $y - $snap_radius, $x + $snap_radius, $y + $snap_radius, -tags => [ 'model' ]);
# 
#     push @{ $view->{snaps} }, [ $x, $y ];
# 
#     return;
# }
# 
# ################################################################################

sub
thing_passes_filter
{
    my ($self, $thing) = @_;

    return 1 unless $thing->{vert};

    my $show = 1;
    for my $dim (0 .. 2) {
        my $max = $thing->{vert}->[$dim];
        $max += $thing->{sizes}->[$dim] if $self->show_filter_includes_dimension;
        $show = $self->filter_by_intersection
            ? ($thing->{vert}->[$dim] <= $self->show_filter_pos_max->dim($dim)
                || $max <= $self->show_filter_pos_min->dim($dim))
            : ($thing->{vert}->[$dim] >= $self->show_filter_pos_min->dim($dim)
                && $max <= $self->show_filter_pos_max->dim($dim));
        last unless $show;
    }

    return $show;
}
 
# ################################################################################
# 
# sub
# refresh_views
# {
# 
#     for my $view (values %flat_view) {
#         $view->{canvas}->delete('model');
#         $view->{snaps} = [];
#         return unless defined $self->size_factor;
#         my $new_vertex_incomplete = 0;
#         if ($new_vertex->{vertex}) {
#             $new_vertex_incomplete = scalar grep { ! defined $_ } @{ $new_vertex->{vertex} };
#         }
#         if ($new_vertex_incomplete) {
# 
#             my @view_coords = model_xyz_to_view_xy($view, @{ $new_vertex->{vertex} } );
# 
#             $view->{canvas}->coords($view->{selected_line_x}, $view_coords[0], 0, $view_coords[0], $view_height);
#             $view->{canvas}->coords($view->{selected_line_y}, 0, $view_coords[1], $view_width, $view_coords[1]);
#         }
#         else {
#             $view->{canvas}->coords($view->{selected_line_x}, 0,0,0,0);
#             $view->{canvas}->coords($view->{selected_line_y}, 0,0,0,0);
#             $view->{canvas}->coords($view->{new_object_indicator}, 0,0,0,0);
#         }
#     }
# 
#     # update the selected flags in @things
#     update_selected_flags();
# 
#     for my $thing (@system_things, @things) {
# 
# #        $log->debug("drawing $thing->{name}: ". Dumper($thing));
# 
#         for my $view (values %flat_view) {
# 
#             if ($view->{render_by_selection}) {
#                 if ($view->{render_unselected}) {
#                     next if $thing->{_selected};
#                 }
#                 else {
#                     next unless $thing->{_selected};
#                 }
#             }
# 
#             if ($filter_shown_by_position) {
#                 next unless thing_passes_filter($thing);
#             }
# 
#             my $xdim = $view->{x_dim};
#             my $ydim = $view->{y_dim};
# 
#             my $color;
#             if ($thing->{_selected}) {
#                 $color = 'white';
#             }
#             else {
#                 $color = color_vec_to_tk($thing->{color});
#             }
# 
#             if ($thing->{type} == $WxBuild::OT_LINE) {
# 
#                 my @view_verts;
#                 for my $vert ( @{ $thing->{verts} } ) {
#                     my ($view_x, $view_y) = model_xyz_to_view_xy($view, @{ $vert });
#                     push @view_verts, $view_x, $view_y;
#                 }
# #                $log->debug("line at @view_verts");
#                 $thing->{_tk}->{$view->{name}} = $view->{canvas}->createLine(@view_verts, -fill => color_vec_to_tk($thing->{color}), -tags => 'model' );
#             }
#             elsif ($thing->{type} == $WxBuild::OT_CUBOID) {
#                 next unless $thing->{solid};
#                 # cuboids have a single vertex and a size array
# #                $log->debug("cuboid at " . join('.',@{ $thing->{vert} }) . " size " . join('.',@{ $thing->{sizes} }) );
#                 my ($base_x, $base_y) = model_xyz_to_view_xy($view, @{ $thing->{vert} });
#                 my @model_corner = (
#                     $thing->{vert}->[0] + $thing->{sizes}->[0],
#                     $thing->{vert}->[1] + $thing->{sizes}->[1],
#                     $thing->{vert}->[2] + $thing->{sizes}->[2],
#                     );
#                 my ($corner_x, $corner_y) = model_xyz_to_view_xy($view, @model_corner);
#                 $thing->{_tk}->{$view->{name}} = $view->{canvas}->createRectangle(
#                     $base_x, $base_y, $corner_x, $corner_y,
#                     -outline => $color, -tags => 'model' );
# 
#                 if ($thing->{_selected}) {
#                     add_snap_target($view, $base_x, $base_y);
#                     add_snap_target($view, $base_x, $corner_y);
#                     add_snap_target($view, $corner_x, $corner_y);
#                     add_snap_target($view, $corner_x, $base_y);
#                 }
# 
#             }
#             elsif ($thing->{type} == $WxBuild::OT_RAMP || $thing->{type} == $WxBuild::OT_STAIRS || $thing->{type} == $WxBuild::OT_PRISM ) {
# 
#                 my $calc_props = $thing->{_transform_props}->{$thing->{transform_type}};
# 
#                 # transformed objects; use the end_vertices arrays to show the shape of the thing
#                 if ($thing->{transform_props}->{$thing->{transform_type}}->{Plane} eq $view->{name}) {
#                     my @base_points;
#                     my $havent_skipped = 1;
#                     for my $vertex (@{ $calc_props->{end_vertices}->[0] }) {
# 
#                         # the first point in a prism is the centre
#                         if ($thing->{type} == $WxBuild::OT_PRISM && $havent_skipped) {
#                             $havent_skipped = 0;
#                             next;
#                         }
# 
#                         # note that all render-ready data must be translated by the thing's vertex.
#                         my ($x,$y) = model_xyz_to_view_xy($view,
#                             $vertex->[0] + $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,
#                             $vertex->[1] + $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,
#                             $vertex->[2] + $thing->{vert}->[2] + $thing->{sizes}->[2] / 2,
#                             );
#                         push @base_points, ($x, $y);
#                     }
#                     $thing->{_tk}->{$view->{name}} = $view->{canvas}->createPolygon(
#                         @base_points,
#                         -outline => $color, -fill => undef, -tags => 'model' );
#                     if ($thing->{_selected}) {
#                         while (@base_points) {
#                             my $x = shift @base_points;
#                             my $y = shift @base_points;
#                             add_snap_target($view, $x, $y);
#                         }
#                     }
#                 }
#                 else {
#                     my $from = $calc_props->{side_points}->[0];
#                     my @snaps = ();
#                     for my $index ( 1 .. $#{ $calc_props->{side_normals} } ) {
# 
#                         my $to = $calc_props->{side_points}->[$index * 2 + 1];
# 
#                         my ($from_x,$from_y) = model_xyz_to_view_xy($view,
#                             $from->[0] + $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,
#                             $from->[1] + $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,
#                             $from->[2] + $thing->{vert}->[2] + $thing->{sizes}->[2] / 2,
#                             );
#                         my ($to_x,$to_y) = model_xyz_to_view_xy($view,
#                             $to->[0] + $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,
#                             $to->[1] + $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,
#                             $to->[2] + $thing->{vert}->[2] + $thing->{sizes}->[2] / 2,
#                             );
# 
#                         $view->{canvas}->createRectangle(
#                             $from_x, $from_y, $to_x, $to_y,
#                             -outline => $color, -tags => 'model' );
# 
#                         $from = $calc_props->{side_points}->[$index * 2];
# 
#                         if ($thing->{_selected}) {
#                             add_snap_target($view, $from_x, $from_y);
#                             add_snap_target($view, $to_x, $to_y);
#                         }
# 
#                     }
#                 }
# 
#             }
# 
#             
#         }
#     }
# 
#     for my $view (values %flat_view) {
#         $view->{canvas}->lower('model','selection');
#     }
# 
#     return;
# }
# 
# ################################################################################
# 
# sub gl_frame_vis_change
# {
#     return unless $gl_frame;
#     queue_gl_draw;
# }
# 
# sub view_frame_exposure_change
# {
#     return unless $gl_frame;
#     queue_gl_draw;
# }
# 
# sub gl_frame_config_change
# {
#     return unless $gl_frame;
#     recalc_views();
#     my $par = shift;
#     my $w = $par->Width;
#     my $h = $par->Height;
#     glpMoveResizeWindow(0,0,$w,$h);
#     glViewport(0,0,$w,$h);
# 
#     queue_gl_draw();
# }
# 
# sub
# gl_zoom_handler
# {
#     my ($frame, $zoom_direction) = @_;
#     $log->debug("zoom handler $zoom_direction");
# 
#     $zoom_direction *= 4 if $control_key_down;
# 
#     # find vector from eye to target and reduce it by some factor, then add it to the eye vector
#     $gl_var->{eye}->{x} += ($gl_var->{eye}->{x} - $gl_var->{target}->{x}) / 5 * $zoom_direction,
#     $gl_var->{eye}->{y} += ($gl_var->{eye}->{y} - $gl_var->{target}->{y}) / 5 * $zoom_direction,
#     $gl_var->{eye}->{z} += ($gl_var->{eye}->{z} - $gl_var->{target}->{z}) / 5 * $zoom_direction,
# 
#     queue_gl_draw();
# 
# }

1;
