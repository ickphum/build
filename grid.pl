#!/usr/bin/perl -w -- 

use Wx 0.15 qw[:allclasses wxTheApp];
use strict;

use Log::Log4perl qw(get_logger);
use Getopt::Long;
use Data::Dumper qw(Dumper);
use FindBin qw($Bin);
use List::Util qw(max min);
use Math::Trig;
use Math::Round;
use Math::VectorReal;
use Math::MatrixReal;
use English qw(-no_match_vars);
use File::Path qw(make_path);

my $log = get_logger;

# main variables {{{1

my %options;

# name ranges
our (
    $NR_BUILD_GRID,
    $NR_THINGS,
    $NR_VERTEX,
    $NR_ADJUSTMENT_BOX,
    $NR_ADJUSTMENT_GRID,
    ) = map { 1 << $_ } (0 .. 15);

# display list ids for fixed items; grids, adjustment box (when we have grouped adjustments),
# and a max offset by which item ids are adjusted during selection.
our (
    $DL_GROUND_GRID,
    $DL_BUILD_GRID,
    $DL_ADJUSTMENT_BOX,
    $DL_X_ADJUSTMENT_GRID,
    $DL_Y_ADJUSTMENT_GRID,
    $DL_Z_ADJUSTMENT_GRID,
    $DL_MAX_OFFSET,
    ) = (1 .. 15);

my $ground_grid = {
    display_list_id => $DL_GROUND_GRID,
    needs_compile => 1,
    offset_v => [ -64, 0, -64 ],
    major_count => 32,
    minor_count => 32,
    cell_size => 4,
    hidden => 1,
    dim => 1,
    color_cycle => [
        [ 1,1,1 ],
#        [ 0.63, 0.63, 0.63 ],
        [ 0.50, 0.50, 0.50 ],
    ],
};

our $TRANSPARENT_ALPHA_01 = 0.1;
our $TRANSPARENT_ALPHA_03 = 0.3;
our $TRANSPARENT_ALPHA_05 = 0.5;
our $TRANSPARENT_ALPHA_07 = 0.7;
our $TRANSPARENT_ALPHA_09 = 0.9;

my $build_grid = {
    display_list_id => $DL_BUILD_GRID,
    needs_compile => 1,
    offset_v => [ -16, 1, -16, ],
    major_count => 64,
    minor_count => 64,
    cell_size => 0.5,
    name_range => $NR_BUILD_GRID,
    dim => 1,

    # records hits from XY position
    selected_index => { },

    # records highlighted cells eg build area
    highlighted_index => { },

    # records selection count for each cell index; positive
    # count means paint with selected_color_cycle colors
    selected_count => [],

    color_cycle => [
        [ 0, 1, 0, $TRANSPARENT_ALPHA_05 ],
        [ 0, 0.5, 0, $TRANSPARENT_ALPHA_05 ],
    ],
    selected_color_cycle => [
        [ 1, 0, 0, $TRANSPARENT_ALPHA_05 ],
        [ 0.5, 0, 0, $TRANSPARENT_ALPHA_05 ],
    ],
    cursor_color_cycle => [
        [ 1, 1, 0, $TRANSPARENT_ALPHA_05 ],
        [ 0.5, 0.5, 0, $TRANSPARENT_ALPHA_05 ],
    ],
};

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

my $control_translation = {
};
my $reverse_control_translation = {
};

# choice settings that should use the string value, not the position,
# probably because the choices are external resources (eg texture or shape files).
my $text_choice_setting = {
    texture => 1,
    shape => 1,
};

my @POPUP_ACTIONS =
my (
    $PA_MOVE,
    $PA_RESIZE,
    $PA_HIDE,
    $PA_HIDE_OTHERS,
    $PA_WIREFRAME,
    $PA_ADJUST,
    $PA_SHOW_VERTICES,
    $PA_DELETE,
    $PA_CENTRE_IN_VIEW,
    $PA_GRID_TO_TOP,
    undef,
    $PA_SHOW_ALL,
    $PA_TOGGLE_GRID,
    $PA_TOGGLE_GROUND,
    $PA_CLEAR,
    $PA_SELECT_ALL,
    $PA_SELECT_NONE,
    $PA_FRONT_VIEW,
    )
    = qw(Move Resize Hide HideOthers Wireframe Adjust ShowVertices Delete CentreInView GridToTop _ ShowAll ToggleGrid ToggleGround Clear SelectAll SelectNone ViewFront);

my @other_dims = (
    [ 1, 2 ],
    [ 0, 2 ],
    [ 0, 1 ],
);

# die handler {{{1

$SIG{__DIE__} = sub {

    my ($msg) = @_;
    
    # do nothing if already in an eval or if parsing; I think this only happens
    # early on (ie while modules are loading) so nothing to save.
    if (! defined $EXCEPTIONS_BEING_CAUGHT || $EXCEPTIONS_BEING_CAUGHT) {
        die @_;
    }

    # save to crash file
    my $i = 0;
    while (-f "gridcrash$i.yml") {
        $i++;
    }
    my $file = "gridcrash$i.yml";

    if (my $save_file = GridApp::save_file(undef, undef, $file)) {
        $msg .= "\nWork saved in $file";
    }

    # continue dying
    die $msg;
};

# main functions {{{1

#*******************************************************************************
sub get_control_value { #{{{2
    my ($control, $setting) = @_;

    my $value;

    if ((ref $control) =~ /ColourPickerCtrl/) {
        my $wx_colour = $control->GetColour;
        my @rgb = ( $wx_colour->Red, $wx_colour->Green, $wx_colour->Blue, ); 
        $value = [ map { $_ / 255 } @rgb ];
    }
    elsif ((ref $control) =~ /Choice/) {
        $value = $control->GetSelection;
        if ($text_choice_setting->{$setting}) {
            $value = $control->GetString($value);
        }
        elsif ($control_translation->{$setting}) {
            $value = $control_translation->{$setting}->{$value};
        }
    }
    else {
        $value = $control->GetValue;
    }

    return $value;
}

#*******************************************************************************
sub set_control_value { #{{{2
    my ($control, $setting, $value) = @_;

    $log->info("undef value for $setting") unless defined $value;

    if ((ref $control) =~ /ColourPickerCtrl/) {
        $control->SetColour(Wx::Colour->new( map { int($_ * 255) } @{ $value } ));
    }
    elsif ((ref $control) =~ /Choice/) {
        if ($text_choice_setting->{$setting}) {
            $value = $control->FindString($value);
        }
        elsif ($reverse_control_translation->{$setting}) {
            $value = $reverse_control_translation->{$setting}->{$value};
        }
        $control->SetSelection($value);
    }
    else {
        $control->SetValue($value);
    }

    return $value;
}

#*******************************************************************************
# Rotate a vertex "up and down" around the origin
sub rotate_vertex_vertically { #{{{2
    my ($vertex, $angle) = @_;

    # find the normal to the plane containing the vertex, a point directly below (or above, if the vertex has y==0)
    # the vertex and the origin.
    my @normal = normalise_vector(triangle_normal(0,0,0,@{ $vertex }, $vertex->[0], $vertex->[1] ? 0 : 1, $vertex->[2]));

    my $sin = sin(deg2rad($angle));
    my $cos = cos(deg2rad($angle));
    my $t = 1 - $cos;
    my ($x, $y, $z) = @normal;

    my $rotation_matrix = Math::MatrixReal->new_from_rows (
        [
            [ $t * $x * $x + $cos, $t * $x * $y - $sin * $z, $t * $x * $z + $sin * $y, 0 ],
            [ $t * $x * $y + $sin * $z, $t * $y * $y + $cos, $t * $y * $z - $sin * $x, 0 ],
            [ $t * $x * $z - $sin * $y, $t * $y * $z + $sin * $x, $t * $z * $z + $cos, 0, ],
            [ 0, 0, 0, 1 ],
        ]
    );

    my $point = Math::MatrixReal->new_from_cols( [ [ @{ $vertex }, 0 ] ] );

    my $product = $rotation_matrix->multiply($point);

    return $product->element(1,1), $product->element(2,1),$product->element(3,1);
}

#*******************************************************************************
# As the name indicates, the vertex is rotated around the specified axis, eg the Y axis
# is the line where X & Z are 0. To rotate in this manner around a line parallel to the axis, 
# translate to the axis, rotate, find the rotation offsets and apply these to the original.
sub rotate_vertex_around_axis { #{{{2
    my ($vertex, $axis, $angle) = @_;

    my @new_vertex;

    $new_vertex[$axis] = $vertex->[$axis];

    # TODO Check Math::Geometry for a builtin...

    if ($axis == 0) {
        $new_vertex[1] = $vertex->[1] * cos(deg2rad($angle)) - $vertex->[2] * sin(deg2rad($angle));
        $new_vertex[2] = $vertex->[1] * sin(deg2rad($angle)) + $vertex->[2] * cos(deg2rad($angle));
    }
    elsif ($axis == 1) {
        $new_vertex[0] = $vertex->[2] * sin(deg2rad($angle)) + $vertex->[0] * cos(deg2rad($angle));
        $new_vertex[2] = $vertex->[2] * cos(deg2rad($angle)) - $vertex->[0] * sin(deg2rad($angle));
    }
    elsif ($axis == 2) {
        $new_vertex[0] = $vertex->[0] * cos(deg2rad($angle)) - $vertex->[1] * sin(deg2rad($angle));
        $new_vertex[1] = $vertex->[0] * sin(deg2rad($angle)) + $vertex->[1] * cos(deg2rad($angle));
    }

    return @new_vertex;
}

#*******************************************************************************
sub radiate_vertex_in_plane { #{{{2
    my ($vertex, $axis, $angle, $length) = @_;

    my $cos = cos(deg2rad($angle));
    my $sin = sin(deg2rad($angle));
    $vertex->[$axis_dim->{$axis}->{x}] += $cos * $length;
    $vertex->[$axis_dim->{$axis}->{y}] += $sin * $length;
#    $log->debug("rvip: angle $angle, sin $sin, cos $cos, length $length");

    return;
}

#*******************************************************************************
sub find_ring_points { #{{{2
    my ($anchor_vertex, $axis, $angle, $radius, $angle_change, $count) = @_;

    my @points = ();

    # note that the anchor vertex will not appear in the output list (unless the ring
    # goes right around and generates it again)
    my @pos = @{ $anchor_vertex };

    $angle += $angle_change;
    for my $index (1 .. $count) {
        radiate_vertex_in_plane(\@pos, $axis, $angle, $radius);
        $angle += $angle_change;
        push @points, [ @pos ];
    }

    return @points;
}

#*******************************************************************************
sub find_stretched_ring_points { #{{{2
    my ($centre_vertex, $axis, $angle, $x_size, $y_size, $angle_change, $count) = @_;

    my @points = ();

    my $x_dim = $axis_dim->{$axis}->{x};
    my $y_dim = $axis_dim->{$axis}->{y};

    $angle += $angle_change;
#    $log->debug("fsrp: centre = " . Dumper($centre_vertex));
    for my $index (1 .. $count) {
        my @pos = @{ $centre_vertex };
        radiate_vertex_in_plane(\@pos, $axis, $angle, 1);
        $pos[$axis_dim->{$axis}->{x}] = $centre_vertex->[$x_dim] + $x_size * ($pos[$x_dim] - $centre_vertex->[$x_dim]);
        $pos[$axis_dim->{$axis}->{y}] = $centre_vertex->[$y_dim] + $y_size * ($pos[$y_dim] - $centre_vertex->[$y_dim]);
#        $log->debug("ring point $index, angle $angle " . Dumper(\@pos));
        $angle += $angle_change;
        push @points, [ @pos ];
    }

    return @points;
}

#*******************************************************************************
sub vector_product  { #{{{2
    my($a,$b,$c,$d,$e,$f)=@_;
    return($b*$f-$c*$e,$c*$d-$a*$f,$a*$e-$b*$d);
}

#*******************************************************************************
sub triangle_normal { #{{{2
    $log->logconfess("not enough points for triangle_normal: " . Dumper(\@_))
        unless scalar @_ == 9;
    my(($ax,$ay,$az),($bx,$by,$bz),($cx,$cy,$cz))=@_;
    my(@AB)=($bx-$ax,$by-$ay,$bz-$az);
    my(@AC)=($cx-$ax,$cy-$ay,$cz-$az);
    return(vector_product(@AB,@AC));
}

#*******************************************************************************
sub vector_length { #{{{2
    $log->logconfess("not enough points for vector_length: " . Dumper(\@_))
        unless scalar @_ == 6;
    my($a,$b,$c,$d,$e,$f)=@_;
    return sqrt(($a-$d)**2 + ($b-$e)**2 + ($c-$f)**2);
}

#*******************************************************************************
sub normalise_vector {
    my($a,$b,$c)=@_;
    my $len = vector_length(0,0,0,$a,$b,$c);
    return ($a/$len, $b/$len, $c/$len);
}

#*******************************************************************************
sub assign_event_handler { #{{{2
    my ($control, $event, $handler) = @_;

#    $log->debug("handle $event for " . $self->name);

    my $event_type = "Wx::Event::$event";

    # find out how many args the event type needs to assign the handler
    my $arg_count = length prototype($event_type);

    my @controls = ($control);
    if ($arg_count == 3) {

        # 3 arg events need the parent as the first arg
        unshift @controls, $control->GetParent;
    }
    elsif ($arg_count == 4) {

        # the 4 arg version is used for handlers which affect a range of controls;
        # not modelled yet
        $log->logdie("no 4 arg events yet");
    }
    elsif ($arg_count != 2) {
        $log->logdie("bad event arg $arg_count");
    }

    # assign the handler
    {
        no strict 'refs';
        &{ $event_type }(@controls, $handler);
    }
}

################################################################################

sub get_rectangle_vertices { #{{{2
    my ($base_vertex, $zero_dim, $dim1, $dim2, $reverse) = @_;

    my @cycle = $reverse
        ? (
            [ 1,0 ],
            [ 1,1 ],
            [ 0,1 ],
        )
        : (
            [ 0,1 ],
            [ 1,1 ],
            [ 1,0 ],
        );

    my @vertices = ($base_vertex);

    for my $corner (@cycle) {
        my $vertex = [ @{ $base_vertex } ];

        $vertex->[ $other_dims[$zero_dim]->[0] ] += $dim1 * $corner->[0];
        $vertex->[ $other_dims[$zero_dim]->[1] ] += $dim2 * $corner->[1];

        push @vertices, $vertex;
    }

    return @vertices;
}

################################################################################

sub hex_color_to_floats { #{{{2
    my ($hex_string) = @_;

    if ($hex_string =~ /\A#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})\z/i) {
        return (hex($1) / 255, hex($2) / 255, hex($3) / 255);
    }
    else {
        $log->logdie("bad hex color: $hex_string");
    }
}

################################################################################

use GridThing;

package GridCanvas; #{{{1

# libraries {{{2

use Wx qw(wxTheApp wxMOUSE_BTN_LEFT wxMOUSE_BTN_RIGHT wxMOUSE_BTN_MIDDLE :bitmap :toolbar);
use Wx::Event qw(EVT_PAINT EVT_SIZE EVT_ERASE_BACKGROUND EVT_IDLE EVT_TIMER EVT_LEFT_DOWN EVT_MOUSE_EVENTS);
# must load OpenGL *before* Wx::GLCanvas
use OpenGL qw(:glconstants :glfunctions :glufunctions);
use OpenGL::Image;
#use OpenGL::Simple::GLUT qw(:all);
use Data::Dumper;
use List::Util qw(max min);
use Time::HiRes qw(gettimeofday tv_interval);
use base qw(Wx::GLCanvas Class::Accessor::Fast);

# variables {{{2

# state ids
my (
    $SI_IDLE,
    $SI_SELECT_BUILD_AREA,
    $SI_SELECT_BUILD_HEIGHT,
    $SI_MOVE_BUILD_PLANE,
    $SI_PAN_SCENE,
    $SI_MOVE_EYE_XZ,
    $SI_ROTATE_EYE,
    $SI_ADJUST_THING,
) = (1 .. 100);

# bitmap flags to describe the nature of a mouse event
our (
    $ME_LEFT_BUTTON,                    # 0001
    $ME_RIGHT_BUTTON,                   # 0002
    $ME_MIDDLE_BUTTON,                  # 0004
    $ME_BUTTON_DOWN,                    # 0008
    $ME_BUTTON_UP,                      # 0010
    $ME_WHEEL_FORWARD,                  # 0020
    $ME_WHEEL_BACK,                     # 0040
    $ME_SHIFT_IS_DOWN,                  # 0080
    $ME_CTRL_IS_DOWN,                   # 0100
    $ME_ALT_IS_DOWN,                    # 0200
    $ME_DOUBLE_CLICK,                   # 0400
) = map { 2 ** $_ } (0 .. 14);

# manual states don't need to be OR-ed together, so
# we don't need to keep them distinct in that fashion.
our $ME_MANUAL_SELECT_BUILD_HEIGHT = 0x8000;
our $ME_MANUAL_IDLE                = 0x8001;
our $ME_MANUAL_ADJUSTING           = 0x8002;

# quickly id non-button events, ie moves, which may have shift, ctrl, etc.
my $ME_BUTTON_EVENT = $ME_LEFT_BUTTON | $ME_RIGHT_BUTTON | $ME_MIDDLE_BUTTON
    | $ME_BUTTON_DOWN | $ME_BUTTON_UP | $ME_WHEEL_FORWARD | $ME_WHEEL_BACK;

# key the state hash by the event bitmap so we can get it directly
my $state = {
    $ME_MANUAL_IDLE => {
        id => $SI_IDLE,
        name => 'SI_IDLE',
        start_mouse_x => 0,
        start_mouse_y => 0,
    },
    $ME_LEFT_BUTTON | $ME_BUTTON_DOWN => {
        id => $SI_SELECT_BUILD_AREA,
        name => 'SI_SELECT_BUILD_AREA',
        off => $ME_LEFT_BUTTON | $ME_BUTTON_UP,
        next_state => $ME_MANUAL_SELECT_BUILD_HEIGHT,
    },
    $ME_MANUAL_SELECT_BUILD_HEIGHT => {
        id => $SI_SELECT_BUILD_HEIGHT,
        name => 'SI_SELECT_BUILD_HEIGHT',
        off => $ME_LEFT_BUTTON | $ME_BUTTON_UP,
    },
    $ME_MIDDLE_BUTTON | $ME_BUTTON_DOWN | $ME_CTRL_IS_DOWN => {
        id => $SI_MOVE_BUILD_PLANE,
        name => 'SI_MOVE_BUILD_PLANE',
        off => $ME_MIDDLE_BUTTON | $ME_BUTTON_UP | $ME_CTRL_IS_DOWN,
    },
    $ME_MIDDLE_BUTTON | $ME_BUTTON_DOWN | $ME_SHIFT_IS_DOWN => {
        id => $SI_PAN_SCENE,
        name => 'SI_PAN_SCENE',
        off => $ME_MIDDLE_BUTTON | $ME_BUTTON_UP | $ME_SHIFT_IS_DOWN,
    },
    $ME_MIDDLE_BUTTON | $ME_BUTTON_DOWN => {
        id => $SI_MOVE_EYE_XZ,
        name => 'SI_MOVE_EYE_XZ',
        off => $ME_MIDDLE_BUTTON | $ME_BUTTON_UP,
    },
    $ME_MANUAL_ADJUSTING => {
        id => $SI_ADJUST_THING,
        name => 'SI_ADJUST_THING',
        off => $ME_LEFT_BUTTON | $ME_BUTTON_UP,
    },
};

my $current_state = $state->{$ME_MANUAL_IDLE};

# accessors {{{2
__PACKAGE__->mk_accessors( qw(timer y_rot init select_buffer_len select_buffer_c viewport_c render_time
    texture_id selection_mode previous_selection_mask screenshot_mode adjustment_grid ) );

#*******************************************************************************

sub new { # {{{2
    my( $class, $parent ) = @_;
    my $self = $class->SUPER::new( $parent, -1, [ -1, -1 ], [ -1, -1 ], 0, "GLCanvas"); 
#        [ 4, 5, 12, 16 ] );
#        [ GLX_RGBA, GLX_DOUBLEBUFFER, GLX_DEPTH_SIZE, 16 ] );

    my $timer = $self->timer( Wx::Timer->new( $self ) );
    $timer->Start( 50 );

    $self->y_rot( 0 );
    $self->previous_selection_mask(0);
    $self->selection_mode(0);

    $self->select_buffer_len(64);
    $self->select_buffer_c( OpenGL::Array->new($self->select_buffer_len, GL_INT) );

    $self->viewport_c( OpenGL::Array->new(4,GL_INT) );
    my $app = wxTheApp;

    EVT_PAINT( $self,
        sub {
#            $log->debug("paint");
            $self->Render( );
        }
    );
    EVT_SIZE( $self,
        sub {
#                $log->debug("size");
            $app->dirty( 1 );
        }
    );
    EVT_IDLE( $self,
        sub {
            return unless $app->dirty or $app->count_fps;
#            $log->debug("idle");
            $self->y_rot( $self->y_rot + 2 ) if $app->rotate_around_y;
            $self->Resize( $self->GetSizeWH );
            $self->Refresh;
        }
    );

    EVT_TIMER( $self, -1,
        sub {
            my( $self, $e ) = @_;
#            $log->debug("timer");

            my $tick = $app->tick;
            $app->tick($tick == 0xffff ? 0 : $tick+1 );

            $app->dirty( 1 );
            Wx::WakeUpIdle;
        }
    );

    EVT_MOUSE_EVENTS( $self,
        sub { 
            my ($self, $event) = @_;

            my $event_flags = 0;
            $event_flags |= $ME_LEFT_BUTTON if $event->Button(wxMOUSE_BTN_LEFT);
            $event_flags |= $ME_RIGHT_BUTTON if $event->Button(wxMOUSE_BTN_RIGHT);
            $event_flags |= $ME_MIDDLE_BUTTON if $event->Button(wxMOUSE_BTN_MIDDLE);
            $event_flags |= $ME_BUTTON_DOWN if $event->ButtonDown;
            $event_flags |= $ME_BUTTON_UP if $event->ButtonUp;
            $event_flags |= $ME_WHEEL_FORWARD if $event->GetWheelRotation > 0;
            $event_flags |= $ME_WHEEL_BACK if $event->GetWheelRotation < 0;
            $event_flags |= $ME_SHIFT_IS_DOWN if $event->ShiftDown;
            $event_flags |= $ME_CTRL_IS_DOWN if $event->ControlDown;
            $event_flags |= $ME_ALT_IS_DOWN if $event->AltDown;
            $event_flags |= $ME_DOUBLE_CLICK if $event->ButtonDClick;

            my ($event_x, $event_y) = ($event->GetX, $event->GetY);
            $self->mouse_event_handler($event_flags, $event_x, $event_y);
        }
    );

    $self->adjustment_grid( {
        display_list_id => $DL_X_ADJUSTMENT_GRID,
        needs_compile => 1,
        offset_v => [ 0,0,0, ],
        major_count => 64,
        minor_count => 64,
        cell_size => 0.5,
        name_range => $NR_ADJUSTMENT_GRID,
        dim => 0,

        # records hits from XY position
        selected_index => { },

        # records highlighted cells eg build area
        highlighted_index => { },

        # records selection count for each cell index; positive
        # count means paint with selected_color_cycle colors
        selected_count => [],

        color_cycle => [
            [ 1, 1, 0, $TRANSPARENT_ALPHA_05 ],
            [ 0.5, 0.5, 0, $TRANSPARENT_ALPHA_05 ],
        ],
        selected_color_cycle => [
            [ 1, 0, 1, $TRANSPARENT_ALPHA_05 ],
            [ 0.5, 0, 0.5, $TRANSPARENT_ALPHA_05 ],
        ],
        cursor_color_cycle => [
            [ 1, 1, 0, $TRANSPARENT_ALPHA_05 ],
            [ 0.5, 0.5, 0, $TRANSPARENT_ALPHA_05 ],
        ],
    });

    return $self;
}

#*******************************************************************************

sub GetContext { # {{{2
    my( $self ) = @_;

    if( Wx::wxVERSION >= 2.009 ) {
        return $self->{context} ||= Wx::GLContext->new( $self );
    } else {
        return $self->SUPER::GetContext;
    }
}

#*******************************************************************************

sub SetCurrent { # {{{2
    my( $self, $context ) = @_;

    if( Wx::wxVERSION >= 2.009 ) {
        return $self->SUPER::SetCurrent( $context );
    } else {
        return $self->SUPER::SetCurrent;
    }
}

#*******************************************************************************

sub mouse_event_handler { # {{{2
    my ($self, $event_flags, $event_x, $event_y) = @_;

    # handle all mouse events centrally because most drag-type events follow a 
    # common pattern, build plane area selection being the exception.

    # events are:
    #
    # left click with possible drag: select an area in the build plane and go to height selection
    # mode, which will also consume mouse motion events until a further left click.
    #
    # mouse wheel forward/back - zoom, ie move eye back/forth along vector towards target
    #
    # shift left drag - move build plane in xz plane
    # shift mouse wheel forward/back - move build plane along y axis
    #
    # ctrl left drag - move eye in xz plane
    # ctrl mouse wheel forward/back - move eye along y axis
    #
    # alt left drag - pan scene in xz plane, ie move target & eye
    #
    # alt mouse wheel forward/back - change build grid cell size

    # these are the types of state we can be in:
    #
    # object selection; during the initial left down-drag-up cycle on the build plane, 
    # we track the initial and current cell and highlight the area defined by these 
    # corners. This is the only time we need to do the costly find-by-xy trick.
    #
    # standard drag tracking; for moving and rotating objects, we just use the x & y
    # increments to alter the setting. We'll record the initial position and translate
    # offset from that position accordingly.
    # 
    # mouse wheel clicks are stateless; they can be acted on immediately

#    $log->debug(sprintf "mouse event flags %04x", $event_flags);

    # many places need this
    my $app = wxTheApp;

    # various callbacks might want this
    $app->current_event($event_flags);

    # init for keyboard operation
    $current_state->{start_mouse_x} ||= 0;
    $current_state->{start_mouse_y} ||= 0;

    my ($width, $height) = $self->GetSizeWH;

    # instantaneous changes for actions that don't have a fixed starting point.
    my $this_x_inc = $event_x - ($current_state->{previous_mouse_x} || 0);
    my $this_y_inc = ($current_state->{previous_mouse_y} || 0) - $event_y ;

    #{{{ handle walk mode
    if ($app->walk_mode) {

        if ($event_flags) {

            if ($event_flags == ($ME_LEFT_BUTTON | $ME_BUTTON_DOWN)) {
                $app->walking(-1);
            }
            elsif ($event_flags == ($ME_LEFT_BUTTON | $ME_BUTTON_DOWN | $ME_SHIFT_IS_DOWN)) {
                $app->walking(1);
            }
            elsif ($event_flags & $ME_LEFT_BUTTON && $event_flags & $ME_BUTTON_UP) {
                $app->walking(0);
            }

        }

        if (($this_x_inc || $this_y_inc) && ! ($event_flags & $ME_CTRL_IS_DOWN)) {

            # motion; adjust viewing direction

            # rotate target around eye's Y axis by the X change
            my @eye = @{ $app->eye };
            my @target = @{ $app->target };

            # translate target to Y axis 
            $target[0] -= $eye[0];
            $target[2] -= $eye[2];

            my @rotated_target = main::rotate_vertex_around_axis(\@target, 1, -$this_x_inc);

            # rotated_target - translated_target is the offset to apply to the target
            $app->move_target( $rotated_target[0] - $target[0], -$this_y_inc * 0.1, $rotated_target[2] - $target[2] );
        }

        # these also done at the end of this function
        $current_state->{previous_mouse_x} = $event_x;
        $current_state->{previous_mouse_y} = $event_y;

        return;
    }
    #}}}

    # are we in a non-idle state? Check for end of state first so manual state transitions
    # use the common state start code
    if ($current_state->{id} != $SI_IDLE) {

        # yes, does the event match our state's off event?
        if ($event_flags == $current_state->{off}) {
            #{{{ finish state

            $log->debug("finished state $current_state->{name}");

            # finish actions
            if ($current_state->{id} == $SI_SELECT_BUILD_AREA) {

                # remove the build grid highlight and selected index
                $build_grid->{highlighted_index} = { };
                $build_grid->{selected_index} = { };

            }
            elsif ($current_state->{id} == $SI_SELECT_BUILD_HEIGHT) {

                if ($app->new_thing->dim_v->[1] == 0) {
                    $app->new_thing->do_delete();
                    $app->refresh_grid_markings;
                }
                else {

                    # finished setting height, make the new thing and any height-matched things opaque
                    $app->new_thing->set_state($GridThing::TS_SELECTED);
                    $app->new_thing(undef);
                    for my $thing ( @{ $app->things } ) {
                        $thing->clear_state($GridThing::TS_TRANSPARENT);
                    }

                }
#                $app->set_status_text('',1);
            }
            elsif ($current_state->{id} == $SI_ADJUST_THING) {
                $log->info("finish SI_ADJUST_THING");
            }

            # special case; left-clicks with no (or little) mouse movement may be object selection.
            # We want to do finish actions (which we've already done) for cleanup
            # purposes, but not go to any subsequent states.
            if ($event_flags & $ME_LEFT_BUTTON
                && $event_flags & $ME_BUTTON_UP
                && (abs($current_state->{start_mouse_x} - $event_x) < 3)
                && (abs($current_state->{start_mouse_y} - $event_y) < 3))
            {
                $log->info("left mouse click, no movement, check for object");
                if ($self->select_thing_from_xy($event_x, $event_y, ~$NR_BUILD_GRID)) {

                    # yep, selected a thing (or something else, eg a vertex; can't have been the grid because
                    # we masked it out) so cancel any potential new state.
                    $log->info("selected a thing");
                    $current_state = $state->{$ME_MANUAL_IDLE};
                    $event_flags = 0;
                }
            }

            # if there's an automatic next state, fake the event flags as required.
            if ($current_state->{next_state}) {
                $event_flags = $current_state->{next_state};
            }
            else {

                # stop coincidences between one state's off & another's on
                $event_flags = 0;
            }

            # go to idle so we're open to automatic changes
            $current_state = $state->{$ME_MANUAL_IDLE};
            #}}}
        }
    }
    else {

        # no, we were idle, so the left button down event didn't start a state,
        # probably because it was an unshifted click off the build grid.
        #{{{
        if ($event_flags & $ME_LEFT_BUTTON
            && $event_flags & $ME_BUTTON_UP
            && $current_state->{start_mouse_x} == $event_x
            && $current_state->{start_mouse_y} == $event_y)
        {

            # we're repeating select_thing_from_xy (on down & up) because the first one
            # was restricted to the build grid items. That was also on a previous event
            # so caching the result would be tricky.
            if (my $selector = $self->select_thing_from_xy($event_x, $event_y)) {
                if ($selector == $build_grid) {

                    # we clicked off the grid then dragged back onto it; just clear the
                    # grid selection info so the next real build is unaffected
                    $build_grid->{selected_index} = {};
                }
                else {

                    # We've clicked on the part of an object that extends above the build
                    # grid. The selection callback has already done the work.
                    # we're already idle, so just clear the event so this event doesn't
                    # trigger any new state (unlikely; a state shouldn't start on left-up).
                    $log->info("found object in idle state; dragged onto object");
                }
            } 
            $event_flags = 0;
        }
        #}}}
    }

    if ($current_state->{id} == $SI_IDLE) {

        # we are idle, ie open to any new state

        if (my $new_state = $state->{$event_flags}) {
            # {{{ the event matches a state, change to that state

            # check for state changes that don't fit the system

            # starting an adjustment looks like a build start, ie left button down.
            # If an object is under adjustment and we're over the adjustment box,
            # there will be a current adjustment segment.
            if ($new_state->{id} == $SI_SELECT_BUILD_AREA && $app->current_segment_id) {
                $new_state = $state->{$ME_MANUAL_ADJUSTING};
            }

            $log->debug("go to state $new_state->{name}");
            $current_state = $new_state;

            if ($current_state->{id} == $SI_SELECT_BUILD_AREA) {

                # we're forcing left-down events to only search for grid cells, not things;
                # if we're over a thing (and the grid), the build state starts but then is aborted (above)
                # on the basis that the mouse didn't move between down & up. I guess this is ok; it gives
                # you the option of selecting an obscured build grid cell by moving the mouse, whereas
                # if we selected things on left-down, it would be impossible to do this. How much do we care?

                if ($self->select_thing_from_xy($event_x, $event_y, $NR_BUILD_GRID) == $build_grid) {

                    # there should be one item marked in the build grid's selected_index
                    # hash, that's the starting point for the build.
                    my @selected_indexes = keys %{ $build_grid->{selected_index} };
                    $log->debug("start build at grid index " . join(',', @selected_indexes ));
                    $log->logdie("not one selected index in build_grid") 
                        if scalar @selected_indexes != 1;

                    # record the starting index in terms of row and column, since we're going
                    # to need it in those terms to highlight the selected area.
                    my $index = $selected_indexes[0];
                    $app->start_build(int($index / $build_grid->{minor_count}), $index % $build_grid->{minor_count});

                }
                else {

                    # we clicked off the build grid, deselect everything
                    # make a copy of the list, deselecting each thing changes it
                    my @selected_things = @{ $app->selected_things };
                    for my $thing (@selected_things) {
                        $thing->clear_state($GridThing::TS_SELECTED);
                    }

                    $current_state = $state->{$ME_MANUAL_IDLE};

                }
                $app->hide_vector_frame;
            }
            elsif ($current_state->{id} == $SI_SELECT_BUILD_HEIGHT) {

                # finished selecting area;
                # add a new thing defined by the current build area and zero height
                $app->finish_build_area;

            }
            elsif ($current_state->{id} == $SI_ADJUST_THING) {

                # we've pressed down on a adjustment box segment, so we're
                # starting adjustment. Display the grid for this adjustment.
                my $adjusted_thing = $app->adjusting_things->[0];
                my ($dim, $side, $type, $index) = $adjusted_thing->decode_segment_id($app->current_segment_id);
                $log->info(sprintf "start SI_ADJUST_THING: %02x = $dim, $side, $type, $index", $app->current_segment_id);
                my $adjustment_grid = $self->adjustment_grid;
                $adjustment_grid->{needs_compile} = 1;
                $adjustment_grid->{dim} = $dim;
                $adjustment_grid->{offset_v} = [ @{ $adjusted_thing->{offset_v} } ];
                $adjustment_grid->{offset_v}->[ $other_dims[$dim]->[0] ] -= $adjustment_grid->{major_count} / 2 * $adjustment_grid->{cell_size};
                $adjustment_grid->{offset_v}->[ $other_dims[$dim]->[1] ] -= $adjustment_grid->{minor_count} / 2 * $adjustment_grid->{cell_size};
                if ($side) {
                    $adjustment_grid->{offset_v}->[$dim] += $adjusted_thing->{dim_v}->[$dim];
                }
            }

            # if there's no off state, we immediately go idle again, eg wheel rotations
            unless ($current_state->{off}) {
                $current_state = $state->{$ME_MANUAL_IDLE};
            }

            #}}}
        }
        else {
#            $log->debug(sprintf "no state matches event %02X", $event_flags);

            # we get spurious hits at x coord -1 or y coord 0 if we don't filter here
            if ($event_x >= 0 && $event_y >= 1) {
                if ($self->select_thing_from_xy($event_x, $event_y, $NR_ADJUSTMENT_BOX)) {
                    $log->debug("selected an adjustment thing on movement at $event_x, $event_y");
                }
                else {
                    if ($app->current_segment_id) {
                        $log->debug("clear adjustment box selection");
                        $app->current_segment_id(0);
                        $app->adjusting_things->[0]->needs_compile(1);
                    }
                }
            }
        }

        # we may have started a state or not; record the mouse position so
        # we can identify non-moving clicks in any state including idle
        $current_state->{start_mouse_x} = $event_x;
        $current_state->{start_mouse_y} = $event_y;
    }
    else {
        # we have an on-going state (we finish above here, so we haven't finished).
        # Handle that state's actions.

        #{{{
        # most non-transitory states track mouse movements
        if (($event_flags & $ME_BUTTON_EVENT) == 0) {

            # total changes, for actions that have a fixed start
            # recall y direction is top to bottom
            my $total_x_inc = $event_x - $current_state->{start_mouse_x};
            my $total_y_inc = $current_state->{start_mouse_y} - $event_y ;

            if ($current_state->{id} == $SI_SELECT_BUILD_AREA) {
                if ($self->select_thing_from_xy($event_x, $event_y, $NR_BUILD_GRID) == $build_grid) {

                    # there should be one item marked in the build grid's selected_index
                    # hash, that's the new corner for the build.
                    $log->debug("continue build at grid index " . join(',', keys %{ $build_grid->{selected_index} }));
                    $log->logdie("not one selected index in build_grid") 
                        if scalar keys %{ $build_grid->{selected_index} } != 1;

                    my $index = (keys %{ $build_grid->{selected_index} })[0];
                    $app->continue_build(int($index / $build_grid->{minor_count}), $index % $build_grid->{minor_count});

                }
                else {

                    # we've dragged off the build grid...
                }
            }
            elsif ($current_state->{id} == $SI_SELECT_BUILD_HEIGHT) {

                # use the current mouse coord in relation to the mouse coord when
                # build area selection finished to set the height of the new thing.

                # build upward only for now (building down involves moving the offset vector)
                if ($total_y_inc >= 0) {

                    # build in increments of the grid size
                    $app->new_thing->{dim_v}->[1] = ($total_y_inc >> 3) * $build_grid->{cell_size};
                    $app->continue_build_height;
                }

            }
            elsif ($current_state->{id} == $SI_MOVE_BUILD_PLANE) {

                $build_grid->{offset_v}->[0] += ($this_x_inc / 4) * $build_grid->{cell_size};
                $build_grid->{offset_v}->[2] -= ($this_y_inc / 4) * $build_grid->{cell_size};
                wxTheApp->refresh_grid_markings;
            }
            elsif ($current_state->{id} == $SI_MOVE_EYE_XZ) {

                # y axis movements increase or decrease the XZ distance to the target
                my $x_len = $app->eye->[0] - $app->target->[0];
                my $z_len = $app->eye->[2] - $app->target->[2];
                if ($this_y_inc > 0) {

                    # avoid getting directly over the target, otherwise we lose the up
                    # vector.
                    if (max($x_len, $z_len) > 0.1) {
                    
                        # decrease len
                        $app->move_eye($x_len * -0.02, undef, $z_len * -0.02);
                    }
                }
                elsif ($this_y_inc < 0) {
                    $app->move_eye($x_len * 0.02, undef, $z_len * 0.02);
                }

                # x axis movements rotate around the target
                $self->y_rot($self->y_rot + $this_x_inc);
            }
            elsif ($current_state->{id} == $SI_PAN_SCENE) {

                # move eye, target in the xz plane
                $app->move_eye($this_x_inc / -4, undef, $this_y_inc / 4);
                $app->move_target($this_x_inc / -4, undef, $this_y_inc / 4);
                $log->debug("eye, target = " . Dumper($app->eye, $app->target));
            }
            else {
                $log->debug("mouse move for $current_state->{name}");
            }
        }
        #}}}
    }

    # stateless actions that work at all times
    #{{{

    if ($event_flags & ($ME_WHEEL_FORWARD | $ME_WHEEL_BACK)) {

        # zoom via mousewheel

        my $wheel_direction = $event_flags & $ME_WHEEL_FORWARD ? -1 : 1;

        if ($event_flags & $ME_CTRL_IS_DOWN) {
            
            # move build plane up & down
            $build_grid->{offset_v}->[1] -= $build_grid->{cell_size} * $wheel_direction;
            $build_grid->{needs_compile} = 1;
        }
        elsif ($event_flags & $ME_SHIFT_IS_DOWN) {

            # move eye up and down
            $app->move_eye(undef, 5 * $wheel_direction, undef);
            $log->debug("eye, target = " . Dumper($app->eye, $app->target));
        }
        elsif ($event_flags & $ME_ALT_IS_DOWN) {

            # change build grid size
#            my $old_x_size = $build_grid->{cell_size} * $build_grid->{major_count};
#            my $old_z_size = $build_grid->{cell_size} * $build_grid->{minor_count};
#            $build_grid->{cell_size} *= ($wheel_direction == 1 ? 2 : 0.5);
#            my $new_x_size = $build_grid->{cell_size} * $build_grid->{major_count};
#            my $new_z_size = $build_grid->{cell_size} * $build_grid->{minor_count};
#            $build_grid->{offset_v}->[0] += ($old_x_size - $new_x_size) / 2;
#            $build_grid->{offset_v}->[2] += ($old_z_size - $new_z_size) / 2;
#
#            wxTheApp->refresh_grid_markings;

            $app->angle(min(170, max(5, $app->angle() + 5 * (($wheel_direction == 1) ? 1 : -1))));
            $log->info("new angle: " . $app->angle);
            $app->dirty(1);
        }
        else {

            # zoom - find vector from eye to target and reduce it by some factor, then add it to the eye vector
            $app->move_eye(
                ($app->eye->[0] - $app->target->[0]) / 5 * $wheel_direction,
                ($app->eye->[1] - $app->target->[1]) / 5 * $wheel_direction,
                ($app->eye->[2] - $app->target->[2]) / 5 * $wheel_direction,
            );

        }
    }
    elsif ($event_flags & $ME_RIGHT_BUTTON && $event_flags & $ME_BUTTON_DOWN) {

        if ($event_flags & ($ME_SHIFT_IS_DOWN | $ME_CTRL_IS_DOWN)) {
#            my $new_angle = $app->angle();
#            $new_angle += 5 * (($event_flags & $ME_SHIFT_IS_DOWN) ? 1 : -1);
            $app->angle(min(90, max(0, $app->angle() + 5 * (($event_flags & $ME_SHIFT_IS_DOWN) ? 1 : -1))));
            $app->dirty(1);
        }
        else {
            $app->hide_vector_frame;

            # display popup menu
            if (my $selector = $self->select_thing_from_xy($event_x, $event_y, ~$NR_BUILD_GRID, 1)) {

                # there should be a single entry in this hash...
                $log->debug("clicked on: " . Dumper($selector));
                my $id = (keys %{ $selector->{selected_index} })[0];
                $app->popup_thing($app->things->[$id]);
                $selector->{selected_index} = {};
            }
            else {
                $log->debug("cleared popup_thing");
                $app->popup_thing(undef);
            }
            $app->frame->display_popup_menu('thing_mnu');
        }
    }
    #}}}

    # common finish actions; we don't care if we're idle
    $current_state->{previous_mouse_x} = $event_x;
    $current_state->{previous_mouse_y} = $event_y;

    return;
}

#*******************************************************************************
sub select_thing_from_xy { # {{{2
    my ( $self, $x, $y, $name_range_mask, $skip_callback ) = @_;

    my $selector = 0;

    my @selection = $self->read_pixels($x, $y, $name_range_mask);

    # high values of R are vector indices, but we're crashing out when they reach 250
    # (we'll need to redesign then, see notes in GridThing::compile).
    return 0 if $selection[0] == 0xFF;

    $log->debug("pixels = @selection");

    my $name_range;

    # the nature of the selection is in the R byte, cell/segment/vertex index is encoded in GB bytes
    my $name_index = $selection[1] * 256 + $selection[2];

    if ($selection[0]) {

        # Red byte set; potentially either a grid, the background, the adjustment box or a vertex
        my $fixed_display_list_id = {
            $build_grid->{display_list_id} => $NR_BUILD_GRID,

            # the adjustment box is part of the thing's display list, but it gets a fixed ID
            # in selection mode.
            $DL_ADJUSTMENT_BOX => $NR_ADJUSTMENT_BOX,
        };

        # Check for the fixed display list ids (values 1-99) first, otherwise it's
        # a vertex.
        unless ($name_range = $fixed_display_list_id->{ $selection[0] }) {
            $log->debug("vertex selector id $selection[0], index $name_index");

            # this is a vertex selection
            $name_range = $NR_VERTEX;
        }
    }
    else {

        # Red byte not set; thing id is encoded in GB bytes
        $name_range = $NR_THINGS;
    }

    if (defined $name_range) {
        if ($selector = wxTheApp->named_things->{$name_range}) {
            $selector->{selected_index}->{$name_index} = 1;

            # this is only used for vertex selection, it identifies the thing
            $selector->{selector} = $selection[0];

            if ($selector->{selection_callback} && ! $skip_callback) {
                $selector->{selection_callback}->($selector);
            }
        }
        else {
            $log->logdie("can't find named thing for $name_range");
        }
    }

    return $selector;
}

#*******************************************************************************

sub Resize { # {{{2
    my( $self, $x, $y ) = @_;

    return unless $self->GetContext;
    wxTheApp->dirty( 0 );

    $self->SetCurrent( $self->GetContext );
    glViewport( 0, 0, $x, $y );

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    my $app = wxTheApp;
    gluPerspective($app->angle, $x/$y, $app->near, $app->far);

    glMatrixMode(GL_MODELVIEW);
}

#*******************************************************************************

sub DESTROY { # {{{2
    my( $self ) = @_;

    if ($self->timer) {
        $self->timer->Stop;
        $self->timer( undef );
    }
}

#*******************************************************************************
# compile the specified display list (normal or selection) for the specified grid
sub compile_grid { # {{{2
    my ($grid, $selection_mode) = @_;

#    $log->debug("compile_grid $selection_mode");

    wxTheApp->dirty(1);

    glNewList($grid->{display_list_id} + ($selection_mode ? 100000 : 0), GL_COMPILE);

    glDisable(GL_TEXTURE_2D);

    glPushMatrix();
    glTranslatef( @{ $grid->{offset_v} } );

    my @normal = (0,0,0);
    $normal[ $grid->{dim} ] = 1;
    glNormal3f(@normal);

    my @grid_dims = @{ $other_dims[ $grid->{dim} ] };

    my $color_cycle_length = scalar @{ $grid->{color_cycle} };

    # assume all colors in the cycle either have or don't have alpha
    my $has_alpha = scalar @{ $grid->{color_cycle}->[0] } == 4;

    my $even_minor_grid = ($grid->{minor_count} & 0x1) == 0;

    my $cursor_index = -1;
    if (defined $grid->{cursor_major} && $grid->{cursor_major} >= 0) {
        $cursor_index = $grid->{cursor_major} * $grid->{minor_count} + $grid->{cursor_minor};
    }

    my $index = 0;
    for my $major_cell ( 0 .. $grid->{major_count} - 1 ) {
        for my $minor_cell ( 0 .. $grid->{minor_count} - 1 ) {

            if ($selection_mode) {

                if (defined $grid->{selected_index}) {

                    # we want to identify (ie color) each grid cell uniquely, so we place them
                    # at 65k+, using the grid display id * 4 as the Red value. This gives us 64 grids up to 512 * 512.
                    glColor3ub( $grid->{display_list_id} * 1, ($index & 0xff00) >> 8, $index & 0xff);

                }
                else {
                    
                    # color the whole grid the same color, we don't select cells.
                    glColor3ub( $grid->{display_list_id} * 1, 0, 0);
                }
            }
            else {

                my $color_index = $index;
                $color_index++ if $major_cell % 2 && $even_minor_grid;
                $color_index %= $color_cycle_length;

                my $use_glow = $index == $cursor_index || $grid->{highlighted_index}->{$index};

                my $color_cycle = $index == $cursor_index
                    ? $grid->{cursor_color_cycle}
                    : $grid->{selected_count}->[$index]
                        ? $grid->{selected_color_cycle}
                        : $grid->{color_cycle};

                if ($has_alpha) {

                    if ($use_glow) {
                        $color_cycle->[$color_index]->[3] = $TRANSPARENT_ALPHA_05 + $grid->{glow};
                    }

                    glColor4f( @{ $color_cycle->[$color_index % $color_cycle_length] } );

                    if ($use_glow) {
                        $color_cycle->[$color_index]->[3] = $TRANSPARENT_ALPHA_05;
                    }
                }
                else {
                    glColor3f( @{ $color_cycle->[$color_index % $color_cycle_length] } );
                }
            }

            my $major_base = $major_cell * $grid->{cell_size};
            my $minor_base = $minor_cell * $grid->{cell_size};

            glBegin(GL_QUADS);
#                glVertex3f( $major_base, 0, $minor_base );
#                glVertex3f( $major_base + $grid->{cell_size}, 0, $minor_base );
#                glVertex3f( $major_base + $grid->{cell_size}, 0, $minor_base + $grid->{cell_size} );
#                glVertex3f( $major_base, 0, $minor_base + $grid->{cell_size} );

                my @vertex = (0,0,0);
                $vertex[ $grid_dims[0] ] = $major_base;
                $vertex[ $grid_dims[1] ] = $minor_base;
                glVertex3f(@vertex);

                $vertex[ $grid_dims[0] ] += $grid->{cell_size};
                glVertex3f(@vertex);

                $vertex[ $grid_dims[1] ] += $grid->{cell_size};
                glVertex3f(@vertex);

                $vertex[ $grid_dims[0] ] = $major_base;
                glVertex3f(@vertex);

            glEnd();

            $index++;
        }
    }
    glPopMatrix();

    glEndList();
}

#*******************************************************************************
# display a grid
sub render_grid { # {{{2
    my ($self,$grid) = @_;

    return if $grid->{hidden};

    my $glow = wxTheApp->glow;

    if ($grid->{needs_compile} || (!defined($grid->{glow}) || $grid->{glow} != $glow)) {
        $grid->{glow} = $glow;
        compile_grid($grid,0);
        compile_grid($grid,1) if $grid->{needs_compile};
        wxTheApp->dirty(1);
        $grid->{needs_compile} = 0;
    }

    glCallList($grid->{display_list_id} + ($self->selection_mode ? 100000 : 0));

    return;
}

#*******************************************************************************

sub InitGL { # {{{2
    my $self = shift;

    return if $self->init;
    return unless $self->GetContext;
    $self->init( 1 );

    my @textures = @{ wxTheApp->texture_names };

    my @texture_ids = glGenTextures_p(scalar @textures);
    $self->texture_id({});

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR);
    glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);

    for my $index (0 .. $#textures) {

        my $img = new OpenGL::Image( source=> 'images/' . $textures[$index] . '.tga' );
        my @image_info = $img->Get(qw(gl_internalformat width height gl_format gl_type));
        my $Tex_Pixels = $img->GetArray();

        glBindTexture(GL_TEXTURE_2D, $texture_ids[$index]);
        $self->texture_id->{$textures[$index]} = $texture_ids[$index];

        # The GLU library helps us build MipMaps for our texture.
        if ((my $gluerr = gluBuild2DMipmaps_c(GL_TEXTURE_2D, @image_info, $Tex_Pixels->ptr()))) {
            $log->logdie("GLULib%s\n" . gluErrorString($gluerr));
        }

    }

    # clockwise winding = front face
    glFrontFace(GL_CW);

    glDepthFunc( GL_LESS );
    glEnable( GL_DEPTH_TEST );
    glEnable( GL_BLEND );
    glEnable( GL_NORMALIZE );

    # smooth is the default
#    glShadeModel(GL_FLAT);

    glEnable( GL_LIGHTING );
#    glLightModelfv_p(GL_LIGHT_MODEL_AMBIENT, 1, 1, 1, 1);
    glEnable(GL_COLOR_MATERIAL);
    glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);

    glLightfv_p(GL_LIGHT0,GL_AMBIENT, 0.3, 0.3, 0.3, 1);
    glLightfv_p(GL_LIGHT0,GL_DIFFUSE, 0.9, 0.9, 0.9, 1);

    glMaterialfv_p(GL_FRONT, GL_SPECULAR, 1, 1, 1, 1);
    glMateriali(GL_FRONT, GL_SHININESS, 128);

    glEnable(GL_LIGHT0);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glSelectBuffer_c($self->select_buffer_len, $self->select_buffer_c->ptr());
}

#*******************************************************************************

sub Render { # {{{2
    my( $self, $name_range_mask ) = @_;

    my $dc = Wx::PaintDC->new( $self );

    return unless $self->GetContext;
    $self->SetCurrent( $self->GetContext );
    $self->InitGL unless $self->init;

    $self->selection_mode(defined($name_range_mask) ? $name_range_mask : 0);

    if ($self->selection_mode) {
        glDisable(GL_LIGHTING);

        # textures are already disabled on a thing-by-thing basis in thing->render, 
        # so we've changed that code instead based on selection_mode.

        # we start numbering from 0,0,0 so clear to white instead
        glClearColor(1,1,1,0);
    }
    elsif ($self->screenshot_mode) {

        # white background for screenshots
        $name_range_mask = 0xff;
        glClearColor(1,1,1,0);
    }
    else {
        glClearColor(0.5,0.5,0.8,0);
        $name_range_mask = 0xff;
    }

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glPushMatrix();

    my $app = wxTheApp;

    # automatically set up vector above eye
#    $app->up->[0] = $app->eye->[0] - $app->target->[0];
#    $app->up->[1] = $app->eye->[1] + 1;
#    $app->up->[2] = $app->eye->[2] - $app->target->[2];
    $app->up->[0] = 0;
    $app->up->[1] = 1;
    $app->up->[2] = 0;

    if ($app->walk_mode && $app->walking) {

        # find vector from eye to target and reduce it by some factor, then add it to the eye vector
        # and the target vector.
        my @movement = ( main::normalise_vector(
            ($app->eye->[0] - $app->target->[0]) * $app->walking,
            ($app->eye->[1] - $app->target->[1]) * $app->walking,
            ($app->eye->[2] - $app->target->[2]) * $app->walking,
        ));

        map { $_ *= 0.25 } @movement;

        $app->move_eye(@movement);
        $app->move_target(@movement);
    }

    #  viewing transformation
    gluLookAt(
        @{ $app->eye },
        @{ $app->target },
        @{ $app->up },
    );

    glTranslatef( @{ $app->target } );
    glRotatef( $self->y_rot, 0, 1, 0 );
    glTranslatef( - $app->target->[0], - $app->target->[1], - $app->target->[2] );

    glLightfv_p(GL_LIGHT0,GL_POSITION, 20, 20, 40, 1);

    $self->render_grid($ground_grid) unless $self->screenshot_mode || $self->selection_mode;

    if ($name_range_mask & $NR_THINGS) {
        for my $thing ( @{ wxTheApp->things } ) {
            $thing->render;
        }
    }

    if ($name_range_mask & $NR_ADJUSTMENT_BOX && $app->adjusting_things) {
        for my $thing ( @{ $app->adjusting_things } ) {
            $thing->render;
        }
    }

    if ($name_range_mask & $NR_BUILD_GRID && ! $self->screenshot_mode) {
        $self->render_grid($build_grid);
    }

    if ($current_state->{id} == $SI_ADJUST_THING && $name_range_mask & $NR_ADJUSTMENT_GRID && ! $self->screenshot_mode) {
        $self->render_grid($self->adjustment_grid);
    }

    glPopMatrix();

    if ($app->count_fps) {
        my $new_time = [ gettimeofday ];
        if (my $old_time = $self->render_time) {
            my $frame_time = tv_interval($old_time, $new_time);
            $log->debug("frame_time $frame_time");
#            $app->set_status_text(1/$frame_time, 1);
        }
        $self->render_time($new_time);
    }

    if ($self->selection_mode) {
        glEnable(GL_LIGHTING);
        $self->previous_selection_mask($name_range_mask);

        # note that we don't call SwapBuffers here; we don't want to display
        # the unlit, untextured version. It just sits in the back buffer until the
        # next render wipes it out.
    }
    elsif (! $self->screenshot_mode) {
        $self->SwapBuffers();

        # when we render the real scene, we want to invalidate the old selection view
        $self->previous_selection_mask(0);
    }

    return;
}

#*******************************************************************************
sub read_pixels { # {{{2
    my( $self, $x, $y, $name_range_mask ) = @_;

    # no restriction by default
    $name_range_mask ||= 0xff;

    # We only need to do the selection render if the mask has changed
    if ($self->previous_selection_mask != $name_range_mask) {
        $self->Render( $name_range_mask );
    }

    glReadBuffer(GL_BACK);
    my (undef, $win_height) = $self->GetSizeWH;
    my @pixels = glReadPixels_p($x, $win_height-$y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE);

    return @pixels;
}

#*******************************************************************************
sub take_screenshot { #{{{2
    my ($self) = @_;

    my ($width, $height) = $self->GetSizeWH;
    my $image = new OpenGL::Image(width => $width, height => $height);
    my ($fmt, $size) = $image->Get('gl_format', 'gl_type');

    # turn off grid & background temporarily
    $self->screenshot_mode(1);

    $self->Render;

    glReadBuffer(GL_BACK);

    OpenGL::glReadPixels_c(0, 0, $width, $height, $fmt, $size, $image->Ptr());

    my $app = wxTheApp;
    my $file = $app->frame->{command_txt}->GetValue || 'grid';
    $file =~ s/.yml//;
    my $filename = sprintf("${file}_eye_%-.3f_%-.3f_%-.3f_target_%-.3f_%-.3f_%-.3f_yrot_%-.3f.tga", @{ $app->eye }, @{ $app->target }, $self->y_rot);

    $image->Save($filename);
    $log->info("saved screenshot to $filename");

    # restore grids
    $self->screenshot_mode(0);

    # as with read_pixels, we don't swap buffers here

    return;
}
################################################################################

package GridFrame; # {{{1

use Wx qw[:everything];
use OpenGL qw(:glconstants :glfunctions :glufunctions);
use base qw(Wx::Frame);
use strict;
use LabelSlider;

use Data::Dumper;

our (
    $ID_CLOSE,
    $ID_PROPERTIES,
    $ID_DRAWING,
    $ID_COUNT_FPS,
    $ID_EYE_ORIGIN,
    $ID_MOVE_EYE,
    $ID_WALK,
    $ID_ROTATE_AROUND_Y,
    $ID_CHANGE_VIEW_SIDE,
    $ID_CHANGE_THING_TYPE,
    $ID_CHANGE_PATTERN,
    $ID_DOUBLE_GRID_X,
    $ID_DOUBLE_GRID_Y,
    $ID_HALVE_GRID_X,
    $ID_HALVE_GRID_Y,
    $ID_INC_GRID_X,
    $ID_INC_GRID_Y,
    $ID_DEC_GRID_X,
    $ID_DEC_GRID_Y,
    $ID_DIVIDE_GRID_X,
    $ID_DIVIDE_GRID_Y,
    $ID_MERGE_GRID_X,
    $ID_MERGE_GRID_Y,
    $ID_TOGGLE_CURSOR,
    $ID_SCREENSHOT,
    $ID_USE_CURSOR,
    $ID_MOVE_TARGET,
    $ID_TOGGLE_WALK,
    )
    =
    ( 1000 .. 1100 );

sub new { # {{{2
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $title  = ""                 unless defined $title;
    $pos    = [ 90, 0 ]       unless defined $pos;
    $size   = [ -1, -1 ]       unless defined $size;
    $name   = ""                 unless defined $name;

    $style = wxDEFAULT_FRAME_STYLE 
        unless defined $style;

    $self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );
    $self->{canvas} = GridCanvas->new($self);
    $self->{command_txt} = Wx::TextCtrl->new($self, -1, "test");

#    $self->{statusbar} = GridStatusBar->new($self);
#    $self->SetStatusBar($self->{statusbar});
#    $self->SetStatusBarPane(1);

    my $cursor_image = Wx::Image->new('images/cursor32_circle2.png', wxBITMAP_TYPE_PNG, 0);
    $cursor_image->SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_X, 15);
    $cursor_image->SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_Y, 15);
    $self->{canvas}->SetCursor(Wx::Cursor->new($cursor_image));

    $self->SetTitle("Grid");

    # toolbar {{{3

    $self->{toolbar} = $self->CreateToolBar(wxTB_VERTICAL);
    $self->{toolbar}->AddTool($ID_CLOSE, "", Wx::Bitmap->new("images/icons/actions/gtk-close.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "Close", "Close the application");
    $self->{toolbar}->AddTool($ID_PROPERTIES, "", Wx::Bitmap->new("images/properties.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_CHECK, "Toggle Properties Window", "Show/Hide the Properties window");
    $self->{toolbar}->AddTool($ID_DRAWING, "", Wx::Bitmap->new("images/drawing.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_CHECK, "Toggle Drawing Window", "Show/Hide the Drawing window");
    $self->{toolbar}->AddTool($ID_COUNT_FPS, "", Wx::Bitmap->new("images/stopwatch.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_CHECK, "Count FPS", "Count framerate");
    $self->{toolbar}->AddTool($ID_EYE_ORIGIN, "", Wx::Bitmap->new("images/eye_origin.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "Eye To Origin", "Return the view to the default position");
#    $self->{toolbar}->AddTool($ID_WALK, "",  Wx::Bitmap->new("images/walk.png", wxBITMAP_TYPE_ANY),
#        wxNullBitmap, wxITEM_CHECK, "Toggle Walk mode", "Toggle the movement of the eye using the mouse");
    $self->{toolbar}->AddTool($ID_MOVE_EYE, "", Wx::Bitmap->new("images/move_eye.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_CHECK, "Toggle Eye movement", "Toggle the movement of the eye via the cursor keys");
    $self->{toolbar}->AddTool($ID_CHANGE_VIEW_SIDE, "", Wx::Bitmap->new("images/view_direction.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "View Side", "Change View Side");
    $self->{toolbar}->AddTool($ID_ROTATE_AROUND_Y, "", Wx::Bitmap->new("images/rotate_around_y.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_CHECK, "Rotate Around Y", "Rotate the view around the Y axis");
    $self->{toolbar}->AddTool($ID_CHANGE_THING_TYPE, "", Wx::Bitmap->new("images/cuboid.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "Thing Type", "Change Thing Type");
    $self->{toolbar}->AddTool($ID_CHANGE_PATTERN, "", Wx::Bitmap->new("images/single.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "Pattern", "Change Pattern");

    $self->{ambient_slider} = Wx::Slider->new($self->{toolbar}, -1, 30, 0, 100, wxDefaultPosition, [20, 100], wxSL_VERTICAL | wxSL_INVERSE);
    $self->{ambient_slider_lbl} = Wx::StaticText->new($self->{toolbar}, -1, 30);
    $self->{ambient_slider}->SetToolTip("Ambient light level");
    Wx::Event::EVT_SCROLL($self->{ambient_slider}, sub { 
        my ($slider, $event) = @_;
        my $value = $slider->GetValue;
        $log->debug("ambient : $value"); 
        glLightfv_p(GL_LIGHT0,GL_AMBIENT, $value / 100, $value / 100, $value / 100, 1);
        $self->{ambient_slider_lbl}->SetLabel($value);
    });
    $self->{toolbar}->AddControl( $self->{ambient_slider} );
    $self->{toolbar}->AddControl( $self->{ambient_slider_lbl} );

    $self->{diffuse_slider} = Wx::Slider->new($self->{toolbar}, -1, 90, 0, 100, wxDefaultPosition, [20, 100], wxSL_VERTICAL | wxSL_INVERSE );
    $self->{diffuse_slider_lbl} = Wx::StaticText->new($self->{toolbar}, -1, 90);
    $self->{diffuse_slider}->SetToolTip("Diffuse light level");
    Wx::Event::EVT_SCROLL($self->{diffuse_slider}, sub { 
        my ($slider, $event) = @_;
        my $value = $slider->GetValue;
        $log->debug("diffuse : $value"); 
        glLightfv_p(GL_LIGHT0,GL_DIFFUSE, $value / 100, $value / 100, $value / 100, 1);
        $self->{diffuse_slider_lbl}->SetLabel($value);
    });
    $self->{toolbar}->AddControl( $self->{diffuse_slider} );
    $self->{toolbar}->AddControl( $self->{diffuse_slider_lbl} );

    my $app = wxTheApp;

    Wx::Event::EVT_TOOL($self, $ID_CLOSE, sub { $app->GetTopWindow->Destroy; });
    Wx::Event::EVT_TOOL($self, $ID_PROPERTIES, sub {
        if ($_[1]->IsChecked) {
            $app->prop_frame->Show;
        }
        else {
            $app->prop_frame->Hide;
        }
    });
    Wx::Event::EVT_TOOL($self, $ID_DRAWING, sub {
        if ($_[1]->IsChecked) {
            $app->draw_frame->Show;
        }
        else {
            $app->draw_frame->Hide;

            # reload the shape list when we close the drawing window.
            # It would be extra nice to auto-select any new shape TODO.
            $app->load_shape_names;
            my $choice = $self->{settings}->{control}->{shape}->{controls}->[0];
            $choice->Clear;
            map { $choice->Append($_); } @{ $app->shape_names };
            $choice->SetSelection(0);
        }
    });
    Wx::Event::EVT_TOOL($self, $ID_COUNT_FPS, sub { $app->count_fps($_[1]->IsChecked); });
    Wx::Event::EVT_TOOL($self, $ID_WALK, sub {
        if ($current_state->{id} == $SI_IDLE || $app->walk_mode) {
            $app->walk_mode($_[1]->IsChecked);
            if ($app->walk_mode) {

#                # we know we're idle so we can switch to walk state safely
#                my ($width, $height) = $app->frame->{canvas}->GetSizeWH;
#                $width >>= 1;
#                $height >>= 1;
#                $app->frame->{canvas}->WarpPointer($width, $height);
#                $app->frame->{canvas}->CaptureMouse;
#
#                # update idle position by faking a mouse event
#                $app->frame->{canvas}->mouse_event_handler(0, $width, $height);
            }
        }
        else {
            $self->{toolbar}->ToggleTool($ID_WALK, 0);
            Wx::MessageDialog->new(undef, "Can't walk unless system is idle")->ShowModal;
        }
    });
    Wx::Event::EVT_TOOL($self, $ID_MOVE_EYE, sub {
        $app->moving_eye($_[1]->IsChecked);
    });

    Wx::Event::EVT_TOOL($self, $ID_EYE_ORIGIN, sub {
        $app->frame->{canvas}->y_rot(0);
        $app->eye( [ 0, $app->initial_eye_pos, $app->initial_eye_pos ] );
        $app->target( [ 0,0,0 ] );
    });
    Wx::Event::EVT_TOOL($self, $ID_ROTATE_AROUND_Y,
        sub {
            my ($self, $event) = @_;
            $app->rotate_around_y($event->IsChecked);
        });
    Wx::Event::EVT_TOOL($self, $ID_CHANGE_VIEW_SIDE, sub { $self->display_tool_menu($_[1], 'view_mnu') });
    Wx::Event::EVT_TOOL($self, $ID_CHANGE_THING_TYPE, sub { $self->display_tool_menu($_[1], 'type_mnu') });
    Wx::Event::EVT_TOOL($self, $ID_CHANGE_PATTERN, sub { $self->display_tool_menu($_[1], 'pattern_mnu') });

    # settings controls {{{3

    $self->{settings}->{panel} = Wx::Panel->new($self, -1, wxDefaultPosition, wxDefaultSize, wxBORDER_RAISED);
#    $self->{settings}->{panel}->SetScrollRate(100,100);

    my %setting_info = (
        angle => { class => 'LabelSlider', value => 0, min => 0, max => 360, },
        axis => { class => 'Choice', list => [ qw(X Y Z) ], selected => 1 },
        color_v => { class => 'ColourPickerCtrl', colour => Wx::Colour->new( map { $_ * 255 } @{ $GridThing::COLOR } )},
        selected_color_v => { class => 'ColourPickerCtrl', colour => Wx::Colour->new( map { $_ * 255 } @{ $GridThing::SELECTED_COLOR } )},
        direction => { class => 'Choice', list => [ qw(North South East West) ], },
        texture => { class => 'Choice', list => [ 'none', @{ $app->texture_names } ],  },
        shape => { class => 'Choice', list => [ @{ $app->shape_names } ],  },
        texture_info => [
            { name => 'tile', label => 'Tile', class => 'CheckBox', value => 1, },
            { name => 'factor', class => 'SpinCtrl', value => 0, min => -20, max => 20, },
        ],
        name => { class => 'TextCtrl', value => '', },
        sides => { class => 'SpinCtrl', value => 8, min => 3, max => 50, },
        wall => { class => 'SpinCtrl', value => 1, min => 1, max => 30, },
        smooth => { class => 'CheckBox', value => 1, },
        steps => { class => 'SpinCtrl', value => 1, min => 1, max => 1000, },
        legs => { class => 'SpinCtrl', value => 5, min => 0, max => 90, },
        square => { class => 'CheckBox', value => 0, },
        stretch => { class => 'CheckBox', value => 0, },
        orient => { class => 'CheckBox', value => 1, },
        climb => { class => 'SpinCtrl', value => 0, min => 0, max => 90, },
        base => { class => 'SpinCtrl', value => 1, min => 1, max => 20, },
        ratio => [
            { name => 'top', class => 'FloatSpinCtrl', value => 1, min => 0.01, max => 100, increment => 0.01 },
            { name => 'bottom', class => 'FloatSpinCtrl', value => 1, min => 0.01, max => 100, increment => 0.01 },
        ],
        gridcount => [
            { name => 'x', class => 'SpinCtrl', value => 3, min => 1, max => 100, },
            { name => 'y', class => 'SpinCtrl', value => 3, min => 1, max => 100, },
            { name => 'z', class => 'SpinCtrl', value => 3, min => 1, max => 100, },
        ],
        gridspace => [
            { name => 'x', class => 'SpinCtrl', value => 2, min => 1, max => 100, },
            { name => 'y', class => 'SpinCtrl', value => 2, min => 1, max => 100, },
            { name => 'z', class => 'SpinCtrl', value => 2, min => 1, max => 100, },
        ],
        rotate => [
            { name => 'x', class => 'SpinCtrl', value => 0, min => 0, max => 360, },
            { name => 'y', class => 'SpinCtrl', value => 0, min => 0, max => 360, },
            { name => 'z', class => 'SpinCtrl', value => 0, min => 0, max => 360, },
        ],
        centred_on => { class => 'TextCtrl', value => '', },
        centred_position => { class => 'Choice', list => [ qw/Radius InnerEdge OuterEdge/ ],  },
        swap => [
            { label => 'X', name => 'x', class => 'CheckBox', value => 0, },
            { label => 'Y', name => 'y', class => 'CheckBox', value => 0, },
            { label => 'Texture', name => 'texture', class => 'CheckBox', value => 0, },
        ],
        edge => [
            { label => 'X', name => 'x', class => 'CheckBox', value => 0, },
            { label => 'Y', name => 'y', class => 'CheckBox', value => 0, },
            { label => 'Z', name => 'z', class => 'CheckBox', value => 0, },
        ],
        ring => [
            { name => 'radius', class => 'SpinCtrl', value => 5, min => 1, max => 999, },
            { name => 'count', class => 'SpinCtrl', value => 6, min => 1, max => 999, },
            { name => 'start', class => 'SpinCtrl', value => 0, min => 0, max => 360, },
            { name => 'angle', class => 'SpinCtrl', value => 0, min => 0, max => 360, },
            { name => 'stretchx', class => 'SpinCtrl', value => 1, min => 1, max => 10, },
            { name => 'stretchy', class => 'SpinCtrl', value => 1, min => 1, max => 10, },
        ],
    );

    # create controls for object creation settings
    my %custom_label = (
        gridcount => 'Grid Count XYZ',
        gridspace => 'Grid Space XYZ',
        gridz => 'Z Count/Space',
        edge => 'Edge Only',
        ring => 'Ring R/C/S/A',
        centred_on => 'Centred On',
        centred_position => 'Centred Position',
        texture_info => 'Texture Tile && Factor',
    );
    my $panel = $self->{settings}->{panel};
    for my $setting (keys %setting_info) {
        my $control_specs = $setting_info{$setting};
        $setting =~ /(.)([a-z]+)/;
        my $label = $custom_label{$setting} || ((uc $1) . $2);

        if ((ref $control_specs) ne 'ARRAY') {
            $control_specs = [ $control_specs ];
        }
#            $log->debug("control_specs = " . Dumper($control_specs));
        my @controls = ();
        for my $spec ( @{ $control_specs } ) {

            if ($spec->{class} eq 'LabelSlider') {
                push @controls, LabelSlider->new($panel, -1, $spec->{value}, $spec->{min}, $spec->{max}, $spec->{stub} || $label,);
            }
            elsif ($spec->{class} eq 'Choice') {
                push @controls, Wx::Choice->new($panel, -1, wxDefaultPosition, wxDefaultSize, $spec->{list} );
                $controls[$#controls]->SetSelection($spec->{selected} || 0);
            }
            elsif ($spec->{class} eq 'ColourPickerCtrl') {
                push @controls, Wx::ColourPickerCtrl->new( $panel, -1, $spec->{colour});
            }
            elsif ($spec->{class} eq 'TextCtrl') {
                push @controls, Wx::TextCtrl->new($panel, -1, $spec->{value}, );
            }
            elsif ($spec->{class} eq 'SpinCtrl') {
                push @controls, Wx::SpinCtrl->new($panel, -1, $spec->{value}, wxDefaultPosition, [ 40, -1 ], wxSP_ARROW_KEYS, 
                    $spec->{min}, $spec->{max} );
            }
            elsif ($spec->{class} eq 'FloatSpinCtrl') {
                push @controls, FloatSpinCtrl->new($panel, -1, $spec->{value}, $spec->{increment}, $spec->{min}, $spec->{max}, );
            }
            elsif ($spec->{class} eq 'CheckBox') {
                push @controls, Wx::CheckBox->new($panel, -1, $spec->{label} || $label );
                $controls[$#controls]->SetValue($spec->{value});
            }

            $controls[$#controls]->Hide;
            $controls[$#controls]->SetName($spec->{name} || '');
            $controls[$#controls]->SetToolTip($spec->{name} || $label);

        }

        $self->{settings}->{control}->{$setting}->{controls} = [ @controls ];

    }

    $self->{v_sizer} = Wx::BoxSizer->new(wxVERTICAL);
    $self->{h_sizer} = Wx::BoxSizer->new(wxHORIZONTAL);
    $self->{settings}->{sizer} = Wx::BoxSizer->new(wxVERTICAL);

    # maintaining a separate list is worth it to get a sensible ordering
    my @settings = qw(name color_v selected_color_v rotate texture texture_info smooth sides axis angle steps direction ratio 
        wall legs base square 
        gridcount gridspace edge ring centred_on centred_position orient climb
        shape stretch swap
        );
    $log->warn("a setting is missing") if @settings != keys %{ $self->{settings}->{control} };

    # assign the event to update settings during object creation
    my %control_event = (
        'Wx::TextCtrl' => 'EVT_TEXT',
        'Wx::SpinCtrl' => 'EVT_SPINCTRL',
        'Wx::Choice' => 'EVT_CHOICE',
        'Wx::ColourPickerCtrl' => 'EVT_COLOURPICKER_CHANGED',
        'Wx::CheckBox' => 'EVT_CHECKBOX',
        'Wx::Slider' => 'EVT_SLIDER',
        LabelSlider => 'EVT_SLIDER',
        FloatSpinCtrl => 'EVT_TEXT',
    );

    for my $setting (@settings) {

        $setting =~ /(.)([a-z]+)/;
        my $label = $custom_label{$setting} || ((uc $1) . $2);
        if ($options{no_labels}) {
            $self->{settings}->{control}->{$setting}->{label} = 0;
        }
        else {

            # create the label for the setting
            if ((ref $setting_info{$setting}) eq 'HASH' && $setting_info{$setting}->{class} =~ /CheckBox|LabelSlider/) {
                $self->{settings}->{control}->{$setting}->{label} = 0;
            }
            else {
                $self->{settings}->{control}->{$setting}->{label} = 
                    Wx::StaticText->new($self->{settings}->{panel}, -1, $label);
                $self->{settings}->{control}->{$setting}->{label}->Hide;
                $self->{settings}->{sizer}->Add($self->{settings}->{control}->{$setting}->{label}, 0, wxEXPAND, 0);
            }
        }

        # add the (hidden) settings controls
        my @controls = @{  $self->{settings}->{control}->{$setting}->{controls} };
        my $control_szr;
        if (scalar @controls <= 2) {
            $control_szr = Wx::BoxSizer->new(wxHORIZONTAL);
        }
        else {
            $control_szr = Wx::GridSizer->new(0,3);
        }
        for my $control ( @controls ) {
            $control_szr->Add($control, 0, wxEXPAND, 0);

            my $control_class = ref $control;

            if (my $event = $control_event{$control_class}) {
                if ($control_class =~ /Wx::/) {
                    main::assign_event_handler($control, $event, \&update_setting_values);
                }
                else {
                    main::assign_event_handler($control->event_control, $event, \&update_setting_values);
                }
            }
            else {
                $log->warn("no event for $control_class");
            }

        }
        $self->{settings}->{sizer}->Add($control_szr, 0, wxEXPAND, 0);
    }

    $self->{settings}->{panel}->SetSizer($self->{settings}->{sizer});

    # window layout {{{3

    $self->{h_sizer}->Add($self->{settings}->{panel}, 0, wxEXPAND, 0);
    $self->{h_sizer}->Add($self->{canvas}, 1, wxEXPAND, 0);
    $self->{v_sizer}->Add($self->{h_sizer}, 1, wxEXPAND, 0);
    $self->{v_sizer}->Add($self->{command_txt}, 0, wxEXPAND, 0);
    $self->SetSizer($self->{v_sizer});
    $self->{v_sizer}->Fit($self);
    $self->Layout();

    $self->SetMinSize(Wx::Size->new(200, 200));
    $self->SetClientSize(Wx::Size->new(640, 510));

    # menus & accelerators {{{3

    $self->create_tool_menu('view_mnu', \&GridApp::change_view_handler, qw(Front Right Back Left) );
    $self->create_tool_menu('type_mnu', \&GridApp::change_type_handler, @GridThing::thing_types );
    $self->create_tool_menu('pattern_mnu', \&GridApp::change_pattern_handler, @GridThing::patterns );
    $self->create_tool_menu('thing_mnu', \&GridApp::thing_popup_handler, @POPUP_ACTIONS );

    # accelerators without menus
    $self->SetAcceleratorTable(
        Wx::AcceleratorTable->new (
            [ wxACCEL_CTRL, 'S', wxID_SAVE ],
            [ wxACCEL_CTRL, 'O', wxID_OPEN ],
            [ wxACCEL_CTRL, 'N', wxID_NEW ],
            [ wxACCEL_NORMAL, 'Q', wxID_EXIT ],
            [ wxACCEL_SHIFT, 'C', wxID_CLEAR ],
            [ wxACCEL_CTRL, 'C', $ID_TOGGLE_CURSOR ],
            [ wxACCEL_CTRL, 'P', $ID_SCREENSHOT ],
            [ wxACCEL_CTRL, 'T', $ID_MOVE_TARGET ],
            [ wxACCEL_CTRL, 'W', $ID_TOGGLE_WALK ],
            [ wxACCEL_NORMAL, WXK_SPACE, $ID_USE_CURSOR ],
            [ wxACCEL_SHIFT, WXK_RIGHT, $ID_DOUBLE_GRID_X ],
            [ wxACCEL_SHIFT, WXK_UP, $ID_DOUBLE_GRID_Y ],
            [ wxACCEL_SHIFT, WXK_LEFT, $ID_HALVE_GRID_X ],
            [ wxACCEL_SHIFT, WXK_DOWN, $ID_HALVE_GRID_Y ],
            [ wxACCEL_NORMAL, WXK_RIGHT, $ID_INC_GRID_X ],
            [ wxACCEL_NORMAL, WXK_UP, $ID_INC_GRID_Y ],
            [ wxACCEL_NORMAL, WXK_LEFT, $ID_DEC_GRID_X ],
            [ wxACCEL_NORMAL, WXK_DOWN, $ID_DEC_GRID_Y ],
            [ wxACCEL_CTRL, WXK_RIGHT, $ID_DIVIDE_GRID_X ],
            [ wxACCEL_CTRL, WXK_UP, $ID_DIVIDE_GRID_Y ],
            [ wxACCEL_CTRL, WXK_LEFT, $ID_MERGE_GRID_X ],
            [ wxACCEL_CTRL, WXK_DOWN, $ID_MERGE_GRID_Y ],
        )
    );

    Wx::Event::EVT_MENU( $self, wxID_SAVE, \&GridApp::save_file );
    Wx::Event::EVT_MENU( $self, wxID_OPEN, \&GridApp::open_file );
    Wx::Event::EVT_MENU( $self, wxID_NEW, \&GridApp::create_default_object );
    Wx::Event::EVT_MENU( $self, wxID_EXIT, sub { $self->Destroy; } );
    Wx::Event::EVT_MENU( $self, wxID_CLEAR, sub {
        for my $index (reverse (0 .. $#{ $app->things })) {
            $app->things->[$index]->do_delete;
        }
    } );
    Wx::Event::EVT_MENU( $self, $ID_DOUBLE_GRID_X, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_DOUBLE_GRID_Y, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_HALVE_GRID_X, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_HALVE_GRID_Y, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_INC_GRID_X, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_INC_GRID_Y, \&GridApp::change_grid_size );
#    Wx::Event::EVT_MENU( $self, $ID_INC_GRID_Y, sub { wxTheApp->move_target( undef, 1, undef ) } );
    Wx::Event::EVT_MENU( $self, $ID_DEC_GRID_X, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_DEC_GRID_Y, \&GridApp::change_grid_size );
#    Wx::Event::EVT_MENU( $self, $ID_DEC_GRID_Y, sub { wxTheApp->move_target( undef, -1, undef ) } );
    Wx::Event::EVT_MENU( $self, $ID_DIVIDE_GRID_X, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_DIVIDE_GRID_Y, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_MERGE_GRID_X, \&GridApp::change_grid_size );
    Wx::Event::EVT_MENU( $self, $ID_MERGE_GRID_Y, \&GridApp::change_grid_size );

    Wx::Event::EVT_MENU( $self, $ID_TOGGLE_WALK, sub { 
        my $app = wxTheApp;
        $app->walk_mode(! $app->walk_mode);
        if ($app->walk_mode) {
            $self->{canvas}->CaptureMouse;
        }
        else {
            $self->{canvas}->ReleaseMouse;
        }
    } );
    Wx::Event::EVT_MENU( $self, $ID_TOGGLE_CURSOR, \&GridApp::toggle_grid_cursor );
    Wx::Event::EVT_MENU( $self, $ID_USE_CURSOR, \&GridApp::use_grid_cursor );

    Wx::Event::EVT_MENU( $self, $ID_MOVE_TARGET, sub { GridApp::thing_popup_handler(wxTheApp->frame, undef, $PA_CENTRE_IN_VIEW) } );
    Wx::Event::EVT_MENU( $self, $ID_SCREENSHOT, sub { $self->{canvas}->take_screenshot(); });

    return $self;
}

#*******************************************************************************

sub show_setting_controls { #{{{2
    my ($self, $type) = @_;

    my $setting_shown = GridThing->TYPE_SETTINGS($type);
    my $pattern_setting_shown = GridThing->PATTERN_SETTINGS(wxTheApp->pattern);

    # go through all the setting types and show the ones we want and hide the others
    for my $setting (sort keys %{ $self->{settings}->{control} }) {

        my $show = $setting_shown->{$setting} || $pattern_setting_shown->{$setting};
        $self->{settings}->{control}->{$setting}->{label}->Show($show) if $self->{settings}->{control}->{$setting}->{label};
        for my $control ( @{ $self->{settings}->{control}->{$setting}->{controls} } ) {
            $control->Show($show);
        }
    }

    $self->{settings}->{control}->{name}->{controls}->[0]->SetValue(wxTheApp->thing_type);

    $self->{settings}->{panel}->Layout;

    return;
}

#*******************************************************************************
sub get_setting_values { #{{{2
    my ($self, $type) = @_;

    $type ||= wxTheApp->thing_type;
    my $setting_shown = GridThing->TYPE_SETTINGS($type);
    my $pattern_setting_shown = GridThing->PATTERN_SETTINGS(wxTheApp->pattern);
    my %setting_value = (
        type => $type,
        pattern => wxTheApp->pattern,
    );

    # go through all the setting types and get the values relevant to our interests
    for my $setting (sort keys %{ $self->{settings}->{control} }) {
        if ( $setting_shown->{$setting} || $pattern_setting_shown->{$setting} ) {
            for my $control ( @{ $self->{settings}->{control}->{$setting}->{controls} } ) {
                my $setting_name = $setting . $control->GetName;
                $setting_value{$setting_name} = main::get_control_value($control, $setting_name);
            }
        }
    }

#    $log->debug("settings: " . Dumper(\%setting_value));
    return \%setting_value;
}

#*******************************************************************************
sub update_setting_values { #{{{2
    my ($panel, $event) = @_;

    # if a build is underway, let the new object know the new setting
    if (my $new_thing = wxTheApp->new_thing) {

        # just get & update all setting values using the GridFrame which is somewhere above us...
        my $parent = $panel->GetParent;
        while ($parent !~ /GridFrame/) {
            $parent = $parent->GetParent;
        }

        my $setting_values = $parent->get_setting_values;
        for my $setting ( keys %{ $setting_values } ) {
            $new_thing->$setting($setting_values->{$setting});
        }
        $new_thing->calculate_for_render;
    }

    # force a label update
    if ((ref $panel) =~ /LabelSlider/) {
        $panel->GetValue;
    }

    return;
}

#*******************************************************************************
sub update_property_values { #{{{2
    my ($panel, $event) = @_;
#    $log->debug("property update: " . ref $event->GetEventObject);
#    return;

    # force a label update
    if ((ref $panel) =~ /LabelSlider/) {
        $panel->GetValue;
    }

    # the container might be the frame
    
    # let the selected object know the new property
    my $thing = wxTheApp->selected_things->[0]
        or $log->logdie("no selected thing on property change");
#    my $property_shown = GridThing->TYPE_SETTINGS($thing->type);

    # just get & update all property values using the GridFrame which is somewhere above us...
    my $parent = $panel->GetParent;
    while ($parent !~ /GridFrame/) {
        $parent = $parent->GetParent;
    }

    # go through all the property types and get the values relevant to our interests
    $log->debug("event object: " . Dumper($event->GetEventObject));

    for my $property (keys %{ $parent->{property}->{control} }) {
        my $property_control = $parent->{property}->{control}->{$property};
        for my $control ( @{ $property_control->{controls} } ) {

            my $match = ((ref $control) =~ /Wx/) 
                ? $control == $event->GetEventObject
                : $control->event_control == $event->GetEventObject;
            if ($match) {
                my $property_name = $property . $control->GetName;
                $thing->$property_name(main::get_control_value($event->GetEventObject, $property_name));
                $thing->calculate_for_render;
                return;
            }
        }

    }

    $log->logconfess("didn't find property control for " . ref $event->GetEventObject);

    return;
}

#*******************************************************************************

sub create_tool_menu { #{{{2
    my ($self, $menu_name, $handler, @items) = @_;
    my $menu = $self->{$menu_name} = Wx::Menu->new("");
    for my $item (@items) {

        # handle separators separately (hee)
        if ($item eq '_') {
            $menu->AppendSeparator;
            next;
        }

        my $image_name = File::Spec->catfile('images', lc "$item.png"); 
        my $menu_item;
        if (-e $image_name) {
            $menu_item = Wx::MenuItem->new(undef, -1, $item);
            $menu_item->SetBitmap(Wx::Bitmap->new($image_name, wxBITMAP_TYPE_ANY));
            $menu->Append($menu_item);
        }
        else {
            $log->debug("no image $image_name");
            $menu_item = $menu->Append(-1, $item);
        }
        Wx::Event::EVT_MENU($self, $menu_item->GetId, $handler);
        wxTheApp->menu_text->{$menu_item->GetId} = $item;
    }

    return;
}

#*******************************************************************************

sub display_tool_menu { #{{{2
    my ($self, $event, $menuname) = @_;
    $log->debug("display_tool_menu @_");
    my $pos = $self->{toolbar}->GetToolPos($event->GetId)
        or $log->logdie("no pos for " . $event->GetId);
    $self->PopupMenu($self->{$menuname}, [ 0, $pos * 28 ]);

    return;
}

#*******************************************************************************
sub display_popup_menu { #{{{2
    my ($self, $menuname) = @_;
    $log->debug("display_popup_menu @_");
    $self->PopupMenu($self->{$menuname}, [-1, -1]);

    return;
}

################################################################################

package GridStatusBar; # {{{1

use Wx qw[:everything];
use base qw(Wx::StatusBar Class::Accessor::Fast);
use Data::Dumper;

__PACKAGE__->mk_accessors( qw(resized) );

sub new { #{{{2
    my( $class, $parent ) = @_;

    my $self = $class->SUPER::new( $parent );

    $self->SetFieldsCount(5);

    # the 32 is to allow for the toolbar width
    $self->SetStatusWidths(200, -1, 24, 24, 32);

    $self->SetStatusStyles(wxSB_NORMAL, wxSB_NORMAL, wxSB_FLAT, wxSB_FLAT, wxSB_FLAT);
    $self->SetStatusText("Build 20x0.5 @ 0,0,0", 0);
    $self->SetStatusText("Message", 1);
#    $self->SetStatusText("E", 2);
    $self->SetStatusText("T", 3);

    Wx::Event::EVT_SIZE( $self, \&on_size );
    Wx::Event::EVT_IDLE( $self, \&on_idle );

    $self->{eye_btn} = Wx::BitmapButton->new($self, -1, 
        Wx::Bitmap->new("images/eye16x16.png", wxBITMAP_TYPE_ANY));

    $self->{target_btn} = Wx::BitmapButton->new($self, -1, 
        Wx::Bitmap->new("images/target16x16.png", wxBITMAP_TYPE_ANY));
    $self->{target_btn}->SetToolTip("0,0,0");

    $self->reposition;

    return $self;
}

#*******************************************************************************

sub on_size { #{{{2
    my ($self, $event) = @_;

    $self->resized(1);
    $self->reposition;
    $event->Skip;

    return;
}

#*******************************************************************************

sub on_idle { #{{{2
    my ($self, $event) = @_;

    if ($self->resized) {
        $log->debug("OnIdle: @_");
        $self->reposition;
    }
    $event->Skip;

    return;
}

#*******************************************************************************

sub reposition { #{{{2
    my ($self) = @_;

    my $rect = $self->GetFieldRect(2);
    $self->{eye_btn}->Move($rect->x, $rect->y - 1);

    $rect = $self->GetFieldRect(3);
    $self->{target_btn}->Move($rect->x, $rect->y - 1);

    $self->resized(0);

    return;
}

################################################################################

package GridApp; # {{{1

use Wx qw[:everything];
use base qw(Wx::App Class::Accessor::Fast);
use Data::Dumper;
use YAML qw(DumpFile LoadFile);
use Storable qw(dclone);

use VectorFrame;
use PropFrame;
use DrawFrame;

__PACKAGE__->mk_accessors( qw(
    frame prop_frame draw_frame vector_frame 
    rotate_around_y 
    tick glow_cache 
    things named_things selected_things adjusting_things popup_thing resizing_things 
    new_thing up
    angle near far 
    current_event 
    menu_text 
    dirty
    undo_stack 
    initial_eye_pos
    group_reference_vector 
    count_fps 
    moving_eye 
    texture_names shape_names
    walking walk_mode 
    build_grid 
    vertex_sphere
    current_segment_id
    ) );

sub new { # {{{2
    my( $class, @args ) = @_;
    my $self = $class->SUPER::new( @args );

    # relates menu ids to text
    $self->menu_text({});

    # load texture names so they're available for the window constructor.
    # Actual texture construction happens at canvas init.
    $self->texture_names([]);
    for my $file (glob 'images/*.tga') {
        if ($file =~ /images\/(.+)\.tga/) {
            push @{ $self->texture_names }, $1;
        }
    }

    # load shape names so they're available for the window constructor.
    $self->load_shape_names;

    # need this before we create the property frame
    $self->things([]);

    $self->tick(0);
    $self->frame( GridFrame->new() );
    $self->prop_frame( PropFrame->new($self->frame, 'Properties', wxDefaultPosition, wxDefaultSize) );
    $self->draw_frame( DrawFrame->new($self->frame, 'Drawing') );

    $self->rotate_around_y(0);
    $self->glow_cache({});
    $self->selected_things([]);

    $self->{eye} = [ 0, 0, 0, ];
    $self->{target} = [ 0, 0, 0, ];
    $self->initial_eye_pos(14);
    $self->eye( [ 0, $self->initial_eye_pos, $self->initial_eye_pos ] );
    $self->target( [ 0, 0, 0 ] );
    $self->up ( [] );
    $self->angle(45);
    $self->near(0.5);
    $self->far(1000);
    $self->build_grid($build_grid);

    # this object exists so we can use the rendering calculations to display spheres
    # at each vertex. This object is hidden so will never be displayed.
    $self->vertex_sphere( GridThing->new({
        name => 'vertex_sphere',
        offset_v => [ -0.5, 5, -0.5, ],
        dim_v => [ 1, 1, 1, ],
        pattern => 'Single',
        color_v => [ .5, 0, 0 ],
        selected_color_v => [ 0, .5, 0 ],
        id => 10000,
        rotatex => 0,
        rotatey => 0,
        rotatez => 0,
        state => $GridThing::TS_HIDDEN,
        texture => 0,
        type => 'Sphere',
        steps => 4,
    }));

    $self->adjusting_things(0);
    $self->current_segment_id(0);

    $self->named_things({
        $NR_BUILD_GRID => $build_grid,
        $NR_THINGS => {
            selection_callback => sub { $self->selection_callback(@_) },
        },
        $NR_ADJUSTMENT_BOX => {
            selection_callback => sub {
                my ($selector) = @_;
                my $segment_id = (keys %{ $selector->{selected_index}})[0];
                $log->info(sprintf("adjustment box selection callback : %04x", $segment_id));

                # mark the selected segment as the current one for highlighting
                my $app = wxTheApp;
                $app->current_segment_id($segment_id);

                # redisplay the adjustment box
                $app->adjusting_things->[0]->needs_compile(1);

                $selector->{selected_index} = {};
            },
        },
        $NR_VERTEX => {
            selection_callback => sub {
                my ($selector) = @_;
                $log->debug("vertex selection callback :" . Dumper($selector)); 

                # we have a thing id and a vertex index (the thing id is off by 99 to avoid the grids, adjustment box, etc)
                my $thing_id = $selector->{selector} - 99;
                my $vertex_index = (keys %{ $selector->{selected_index} })[0];

                # record this vertex selection
                $selector->{thing_vertex}->{$thing_id} = $vertex_index;

                # if we now have vertices for two things, we can do the join
                if (scalar keys %{ $selector->{thing_vertex} } == 2) {

                    my @thing_ids = keys %{ $selector->{thing_vertex} };

                    # we join the first thing selected to the second (this one)
                    my ($moving, $target) = $thing_ids[0] == $thing_id 
                        ? (1,0)
                        : (0,1);
                    $log->debug("thing_ids @thing_ids, thing_id $thing_id, moving $moving, target $target");
                    my $moving_thing = $self->things->[$thing_ids[$moving]]
                        or return;
                    my $moving_vertex = $selector->{thing_vertex}->{$moving_thing->id};
                    my $target_thing = $self->things->[$thing_ids[$target]];
                    my $target_vertex = $selector->{thing_vertex}->{$target_thing->id};

                    # recall that vertexes are relative to thing, so have to be converted by offset to be relative to world and
                    # thus comparable.
                    my @move_vector = map { ($target_thing->offset_v->[$_] + $target_thing->{render_calc}->{vertex_list}->[$target_vertex]->[$_]) - 
                        ($moving_thing->offset_v->[$_] + $moving_thing->{render_calc}->{vertex_list}->[$moving_vertex]->[$_])  } ( 0 .. 2);

                    $log->debug("join " . $moving_thing->name . ' onto ' . $target_thing->name . ", vector = @move_vector");

                    # if the moving thing is selected, unselect it temporarily to keep the grid
                    # markings correct;
                    my $moving_is_selected = $moving_thing->state & $GridThing::TS_SELECTED;
                    $moving_thing->clear_state($GridThing::TS_SELECTED) if $moving_is_selected;
                    map { $moving_thing->offset_v->[$_] += $move_vector[$_] } (0 .. 2);
                    $moving_thing->set_state($GridThing::TS_SELECTED) if $moving_is_selected;

                    $moving_thing->clear_state($GridThing::TS_SHOW_VERTICES);
                    $target_thing->clear_state($GridThing::TS_SHOW_VERTICES);

                    delete $selector->{thing_vertex};

                }

                # clear the selection buffer
                $selector->{selected_index} = {};
            },
        },
    });

    # don't call the accessor, it'll try to redraw the setting list and we haven't
    # defined thing_type yet (one of these has to go first)
    $self->{pattern} = $GridThing::patterns[0];
    $self->thing_type($GridThing::thing_types[0]);

    $self->SetTopWindow($self->frame);
    $self->frame->Show(1);
    $self->frame->{command_txt}->SetFocus;

    return $self;
}

#*******************************************************************************
sub load_shape_names { #{{{2
    my ($self) = @_;

    $self->shape_names([]);

    for my $file (glob 'shapes/*.2d') {
        if ($file =~ /shapes\/(.+)\.2d/) {
            push @{ $self->shape_names }, $1;
        }
    }
}

#*******************************************************************************
sub finish_build_area { #{{{2
    my ($self) = @_;

    $log->debug("new thing of type " . $self->thing_type);
    my @x_bounds = sort { $a <=> $b } ($build_grid->{start_x}, $build_grid->{next_x});
    my @z_bounds = sort { $a <=> $b } ($build_grid->{start_z}, $build_grid->{next_z});
    my $settings = $self->thing_type_settings;
#    $log->debug("settings = " . Dumper($settings));
    my $thing = GridThing->new({
        offset_v => [
            $build_grid->{offset_v}->[0] + $x_bounds[0] * $build_grid->{cell_size},
            $build_grid->{offset_v}->[1],
            $build_grid->{offset_v}->[2] + $z_bounds[0] * $build_grid->{cell_size},
        ],
        dim_v => [
            ($x_bounds[1] - $x_bounds[0] + 1) * $build_grid->{cell_size},
            0,
            ($z_bounds[1] - $z_bounds[0] + 1) * $build_grid->{cell_size},
        ],
        %{ $settings },
    });
    $self->add_thing($thing);

    return;
}

#*******************************************************************************
sub start_build { #{{{2
    my ($self, $start_x, $start_z) = @_;

    $build_grid->{start_x} = $start_x;
    $build_grid->{start_z} = $start_z;
    $log->debug("start build coords are $build_grid->{start_x}, $build_grid->{start_z}");

    # in case we go straight to height select without moving the mouse, 
    # record this position as the initial final position as well
    $build_grid->{next_x} = $build_grid->{start_x} ;
    $build_grid->{next_z} = $build_grid->{start_z} ;

    # start the display of the selected area with the new selection
    $build_grid->{highlighted_index} = { $start_x * $build_grid->{minor_count} + $start_z => 1 }; 

    # clear the selected index hash so we can id the new index if we drag
    $build_grid->{selected_index} = {};

    return;
}

#*******************************************************************************
sub continue_build { #{{{2
    my ($self, $next_x, $next_z) = @_;

    # record the index in terms of row and column, since we're going
    # to need it in those terms to highlight the selected area.
    $build_grid->{next_x} = $next_x;
    $build_grid->{next_z} = $next_z;

    $log->debug("continue build coords are $build_grid->{next_x}, $build_grid->{next_z}");

    # we have to clear the highlight hash and reset to the block of cells
    # defined by the new location
    $build_grid->{highlighted_index} = {};
    my @x_bounds = sort { $a <=> $b } ($build_grid->{start_x}, $build_grid->{next_x});
    my @z_bounds = sort { $a <=> $b } ($build_grid->{start_z}, $build_grid->{next_z});
    for my $x ($x_bounds[0] .. $x_bounds[1]) {
        for my $z ($z_bounds[0] .. $z_bounds[1]) {
            $build_grid->{highlighted_index}->{ $x * $build_grid->{minor_count} + $z } = 1;
        }
    }
#    $self->set_status_text(sprintf( "%dx%d",  
#        $x_bounds[1] - $x_bounds[0] + 1,
#        $z_bounds[1] - $z_bounds[0] + 1,
#        ), 1);

    # clear the selected index hash
    $build_grid->{selected_index} = {};

    $build_grid->{needs_compile} = 1;

    return;
}

#*******************************************************************************
sub continue_build_height { #{{{2
    my ($self) = @_;

    $self->new_thing->calculate_for_render;
#    $self->set_status_text($self->new_thing->{dim_v}->[1], 1);

    my $new_height = $self->new_thing->{offset_v}->[1] + $self->new_thing->{dim_v}->[1];
    for my $thing ( @{ $self->things } ) {
        $thing->clear_state($GridThing::TS_TRANSPARENT);
        next if $thing == $self->new_thing || ! ($thing->state & $GridThing::TS_SELECTED);
        if (($thing->{offset_v}->[1] + $thing->{dim_v}->[1]) == $new_height) {
            $thing->set_state($GridThing::TS_TRANSPARENT);
        }
    }

    return;
}

#*******************************************************************************
# Show/hide the build cursor
sub toggle_grid_cursor { #{{{2
    my ($frame, $event) = @_;
    my $self = wxTheApp;

    if (defined $build_grid->{cursor_x}) {

        # once the cursor's been shown, we hide it by setting it -ve so we come back at the same spot
        $build_grid->{cursor_x} *= -1;
        $build_grid->{cursor_z} *= -1;

        # if the build grid has changed dimension, reset to 00
        if ($build_grid->{cursor_x} >= $build_grid->{major_count}) {
            $build_grid->{cursor_x} = undef;
        }
        if ($build_grid->{cursor_z} >= $build_grid->{minor_count}) {
            $build_grid->{cursor_z} = undef;
        }
    }

    $build_grid->{cursor_x} = int($build_grid->{major_count} / 2)
        unless defined $build_grid->{cursor_x};
    $build_grid->{cursor_z} = int($build_grid->{minor_count} / 2)
        unless defined $build_grid->{cursor_z};

    return;
}

#*******************************************************************************
# Take a keyboard event and change/use the grid cursor
sub use_grid_cursor { #{{{2
    my ($frame, $event) = @_;
    my $app = wxTheApp;

    my $event_id = $event->GetId;

    my %grid_movement = (
#        $ID_DOUBLE_GRID_X => [ $build_grid->{major_count} * 2, undef ],
#        $ID_DOUBLE_GRID_Y => [ undef, $build_grid->{minor_count} * 2 ],
#        $ID_HALVE_GRID_X => [ int($build_grid->{major_count}/2), undef ],
#        $ID_HALVE_GRID_Y => [ undef, int($build_grid->{minor_count}/2) ],
        $ID_INC_GRID_X => [ 1,0 ],
        $ID_INC_GRID_Y => [ 0,-1 ],
        $ID_DEC_GRID_X => [ -1,0 ],
        $ID_DEC_GRID_Y => [ 0,1 ],
    );

    if (my $movement = $grid_movement{$event_id}) {
        if ($current_state->{id} == $SI_SELECT_BUILD_HEIGHT) {

            # bit of a hack to make up/down natural
            $app->new_thing->{dim_v}->[1] -= $movement->[1];
            $app->new_thing->{dim_v}->[1] = 0 if $app->new_thing->{dim_v}->[1] < 0;
            $app->continue_build_height;
        }
        else {
            $build_grid->{cursor_x} += $movement->[0];
            $build_grid->{cursor_z} += $movement->[1];
            $build_grid->{needs_compile} = 1;
            $log->debug("cursor now at $build_grid->{cursor_x}, $build_grid->{cursor_z}");

            if ($current_state->{id} == $SI_SELECT_BUILD_AREA) {
                $app->continue_build($build_grid->{cursor_x}, $build_grid->{cursor_z});
            }
        }
    }
    elsif ($event_id == $ID_USE_CURSOR) {

        my $mouse_x = $current_state->{previous_mouse_x};
        my $mouse_y = $current_state->{previous_mouse_y};
        my $state_id = $current_state->{id};

        if ($current_state->{id} == $SI_IDLE) {
            $current_state = $state->{$ME_LEFT_BUTTON | $ME_BUTTON_DOWN};
            $app->start_build($build_grid->{cursor_x}, $build_grid->{cursor_z});
        }
        elsif ($current_state->{id} == $SI_SELECT_BUILD_AREA) {
            $current_state = $state->{$ME_MANUAL_SELECT_BUILD_HEIGHT};
            $app->finish_build_area($build_grid->{cursor_x}, $build_grid->{cursor_z});

            # remove the build grid highlight and selected index
            $build_grid->{highlighted_index} = { };
            $build_grid->{selected_index} = { };
        }
        elsif ($current_state->{id} == $SI_SELECT_BUILD_HEIGHT) {
            $current_state = $state->{$ME_MANUAL_IDLE};

            # finished setting height, make the new thing and any height-matched things opaque
            $app->new_thing->set_state($GridThing::TS_SELECTED);
            $app->new_thing(undef);
            for my $thing ( @{ $app->things } ) {
                $thing->clear_state($GridThing::TS_TRANSPARENT);
            }
#            $app->set_status_text('',1);
        }

        # set the mouse positions for the new state in case the mouse is moved
        if ($current_state->{id} != $state_id) {
            $current_state->{start_mouse_x} = $current_state->{previous_mouse_x} = $mouse_x;
            $current_state->{start_mouse_y} = $current_state->{previous_mouse_y} = $mouse_y;
        }

    }

    return;
}

#*******************************************************************************
# Create a default object
sub create_default_object { #{{{2
    my ($frame, $event) = @_;
    my $self = wxTheApp;

    $log->debug("new thing of type " . $self->thing_type);
    my @x_bounds = (15, 20);
    my @z_bounds = (15, 50);
    my $settings = $self->thing_type_settings;
    $log->debug("settings = " . Dumper($settings));
    my $cube = GridThing->new({
        offset_v => [
            $build_grid->{offset_v}->[0] + $x_bounds[0] * $build_grid->{cell_size},
            $build_grid->{offset_v}->[1],
            $build_grid->{offset_v}->[2] + $z_bounds[0] * $build_grid->{cell_size},
        ],
        dim_v => [
            ($x_bounds[1] - $x_bounds[0] + 1) * $build_grid->{cell_size},
            10 * $build_grid->{cell_size},
            ($z_bounds[1] - $z_bounds[0] + 1) * $build_grid->{cell_size},
        ],
        %{ $settings },
    });
    $self->add_thing($cube);
    $self->new_thing->set_state($GridThing::TS_SELECTED);
    $self->new_thing(undef);
    for my $thing ( @{ $self->things } ) {
        $thing->clear_state($GridThing::TS_TRANSPARENT);
    }
#    $self->set_status_text('',1);
}

#*******************************************************************************
# change grid size according to id of menu event
sub change_grid_size { # {{{2
    my ($frame, $event) = @_;

    my $event_id = $event->GetId;

    my %new_grid_offset = (
        $ID_INC_GRID_X => [ 1, undef ],
        $ID_INC_GRID_Y => [ undef, -1 ],
        $ID_DEC_GRID_X => [ -1, undef ],
        $ID_DEC_GRID_Y => [ undef, 1 ],
    );

    my $app = wxTheApp;

    if ($app->moving_eye) {
        if (my $movement = $new_grid_offset{$event_id}) {
            $app->move_eye($movement->[0] || 0, 0, $movement->[1] || 0);
        }
        return;
    }

    # if the cursor is displayed, you can't change the grid size, the keys move the
    # cursor/selected area instead
    if (defined $build_grid->{cursor_x} && $build_grid->{cursor_x} >= 0) {
        use_grid_cursor($frame,$event);
        return;
    }

    # store current grid settings so we can make changes as required and
    # then work out offset change at the end
    my ($major_count, $minor_count, $cell_size) = @{ $build_grid }{qw(major_count minor_count cell_size)};

    my %new_grid_count = (
        $ID_DOUBLE_GRID_X => [ $build_grid->{major_count} * 2, undef ],
        $ID_DOUBLE_GRID_Y => [ undef, $build_grid->{minor_count} * 2 ],
        $ID_HALVE_GRID_X => [ int($build_grid->{major_count}/2), undef ],
        $ID_HALVE_GRID_Y => [ undef, int($build_grid->{minor_count}/2) ],
#        $ID_INC_GRID_X => [ $build_grid->{major_count} + 2, undef ],
#        $ID_INC_GRID_Y => [ undef, $build_grid->{minor_count} + 2 ],
#        $ID_DEC_GRID_X => [ List::Util::max($build_grid->{major_count} - 2, 2), undef ],
#        $ID_DEC_GRID_Y => [ undef, List::Util::max($build_grid->{minor_count}-2,2) ],
    );

    if (my $new_count = $new_grid_count{$event_id}) {

        # make change to grid dim
        if ($new_count->[0]) {
            $build_grid->{major_count} = $new_count->[0];
        }
        if ($new_count->[1]) {
            $build_grid->{minor_count} = $new_count->[1];
        }
    }
    elsif (my $new_offset = $new_grid_offset{$event_id}) {

        # make change to grid offset
        if ($new_offset->[0]) {
            $build_grid->{offset_v}->[0] += $new_offset->[0] * $cell_size;
        }
        if ($new_offset->[1]) {
            $build_grid->{offset_v}->[2] += $new_offset->[1] * $cell_size;
        }
    }
    else {

        # check for divide or merge
        if ($event_id == $ID_MERGE_GRID_X || $event_id == $ID_DIVIDE_GRID_X) {
            return if $event_id == $ID_DIVIDE_GRID_X && $build_grid->{major_count} == 2;
            $build_grid->{major_count} *= $event_id == $ID_MERGE_GRID_X ? 2 : 0.5;
            $build_grid->{minor_count} *= $event_id == $ID_MERGE_GRID_X ? 2 : 0.5;
            $build_grid->{cell_size} *= $event_id == $ID_MERGE_GRID_X ? 0.5 : 2;
#            $build_grid->{cell_size} *= $major_count / $build_grid->{major_count};
#            $build_grid->{minor_count} = Math::Round::round ( $minor_count * $cell_size / $build_grid->{cell_size});
        }
        elsif ($event_id == $ID_MERGE_GRID_Y || $event_id == $ID_DIVIDE_GRID_Y) {
            return if $event_id == $ID_MERGE_GRID_Y && $build_grid->{minor_count} == 2;
            $build_grid->{minor_count} *= $event_id == $ID_MERGE_GRID_Y ? 0.5 : 2;
            $build_grid->{major_count} *= $event_id == $ID_MERGE_GRID_Y ? 0.5 : 2;
#            $build_grid->{cell_size} *= $minor_count / $build_grid->{minor_count};
#            $build_grid->{major_count} = Math::Round::round ( $major_count * $cell_size / $build_grid->{cell_size});
        }
        else {
            $log->info("unknown event $event_id");
        }
    }

    # change offset to keep grid centre constant(ish)
    $build_grid->{offset_v}->[0] -= ($build_grid->{major_count} / 2 * $build_grid->{cell_size}) - ($major_count / 2 * $cell_size);
    $build_grid->{offset_v}->[2] -= ($build_grid->{minor_count} / 2 * $build_grid->{cell_size}) - ($minor_count / 2 * $cell_size);

    $app->refresh_grid_markings;

    return;
}

#*******************************************************************************
# build a hash of constructor args for the thing constructor from the setting
# controls
sub thing_type_settings { # {{{2
    my ($self) = @_;

    return $self->frame->get_setting_values($self->thing_type);
}

#*******************************************************************************

sub thing_type { # {{{2
    my ($self, $thing_type) = @_;

    if ($thing_type) {
        $self->{thing_type} = $thing_type;

        # reset the settings controls to show those applicable to the new type
        $self->frame->show_setting_controls($thing_type);
    }

    return $self->{thing_type};
}

#*******************************************************************************
sub pattern { # {{{2
    my ($self, $pattern) = @_;

    if ($pattern) {
        $self->{pattern} = $pattern;

        # reset the settings controls to show those applicable to the new type
        $self->frame->show_setting_controls($self->thing_type);
    }

    return $self->{pattern};
}

#*******************************************************************************
sub selection_callback { # {{{2
    my ($self, $selector) = @_;

    # process selection changes
    $log->debug("selection_callback: selector " . Dumper($selector));

    my $event = $self->current_event;

    # build a temporary hash of selected things
    my $selected_thing = { map { $_->id => $_ } @{ $self->selected_things } };

    # unmodified selection clears all previous selections
    unless ($event & ($GridCanvas::ME_SHIFT_IS_DOWN | $GridCanvas::ME_CTRL_IS_DOWN)) {
        $log->debug("clear selection");
        for my $index (keys %{ $selected_thing }) {

            # if a thing was re-selected by this event, don't clear selection
            next if $selector->{selected_index}->{$index};

            $self->things->[$index]->clear_state($GridThing::TS_SELECTED);
        }
    }

    # find things selected by this event
    my @selected_indexes = keys %{ $selector->{selected_index} };
    $log->debug("selected_indexes = @selected_indexes");
    for my $index (@selected_indexes) {
        if ($event & $GridCanvas::ME_CTRL_IS_DOWN) {
            $self->things->[$index]->toggle_state($GridThing::TS_SELECTED);
        }
        else {

            # shift-click; select everything between here and previous selected thing.
            # Do this first so properties are displayed for clicked thing, not 
            # last intervening thing.
            if ($event & $GridCanvas::ME_SHIFT_IS_DOWN) {
                my $lower_index = $index - 1;
                while ($lower_index >= 0) {
                    if ( $selected_thing->{$lower_index} ) {
                        for my $intervening_index ($lower_index + 1 .. $index - 1) {
                            $self->things->[$intervening_index]->set_state($GridThing::TS_SELECTED);
                        }
                        last;
                    }
                    $lower_index--;
                }
            }

            # simple or shift click; select thing unless already selected
            $self->things->[$index]->set_state($GridThing::TS_SELECTED)
                unless $selected_thing->{$index};

        }
    }

    # clear selected_index so we can see changes
    $selector->{selected_index} = {};

    $self->hide_vector_frame;

    return;
}

#*******************************************************************************

sub eye { #{{{2
    my ($self, $vector) = @_;

    if ($vector) {
        $self->move_eye($vector->[0] - $self->{eye}->[0],
            $vector->[1] - $self->{eye}->[1],
            $vector->[2] - $self->{eye}->[2],
        );
    }

    return $self->{eye};
}

#*******************************************************************************

sub target { #{{{2
    my ($self, $vector) = @_;

    if ($vector) {
        $self->move_target($vector->[0] - $self->{target}->[0],
            $vector->[1] - $self->{target}->[1],
            $vector->[2] - $self->{target}->[2],
        );
    }

    return $self->{target};
}

#*******************************************************************************

sub move_eye { #{{{2
    my ($self, @inc) = @_;
    $log->debug("move_eye: " . Dumper(\@inc));

    for my $dim (0 .. 2) {
        $self->{eye}->[$dim] += $inc[$dim] if defined $inc[$dim];
    }
#    $self->frame->{statusbar}->{eye_btn}->SetToolTip(sprintf "%.2f,%.2f,%.2f",
#        $self->{eye}->[0],$self->{eye}->[1],$self->{eye}->[2]);

    return;
}

#*******************************************************************************

sub move_target { #{{{2
    my ($self, @inc) = @_;

    for my $dim (0 .. 2) {
        $self->{target}->[$dim] += $inc[$dim] if defined $inc[$dim];
    }
    $log->debug("new target: $self->{target}->[0], $self->{target}->[1], $self->{target}->[2]");
#    $self->frame->{statusbar}->{target_btn}->SetToolTip(sprintf "%.2f,%.2f,%.2f", 
#        $self->{target}->[0],$self->{target}->[1],$self->{target}->[2]);

    return;
}

#*******************************************************************************
sub get_selected_extent { # {{{2
    my ($self) = @_;

    my $offset_v = [undef,0,undef];
    my $dim_v = [undef,0,undef];
    for my $thing ( @{ $self->selected_things } ) {
        for my $dim (0 , 2) {
            $offset_v->[$dim] = $thing->offset_v->[$dim]
                if (!defined $offset_v->[$dim] || $thing->offset_v->[$dim] < $offset_v->[$dim]);
            # get the actual max extent in real terms, not the dimension
            $dim_v->[$dim] = $thing->offset_v->[$dim] + $thing->dim_v->[$dim]
                if (!defined $dim_v->[$dim] || ($thing->offset_v->[$dim] + $thing->dim_v->[$dim]) > $dim_v->[$dim]);
        }
    }
    for my $dim (0 .. 2) {
        $dim_v->[$dim] -= $offset_v->[$dim];
    }

    return $offset_v, $dim_v;
}

#*******************************************************************************
sub thing_popup_handler { # {{{2
    my ($frame, $event, $action) = @_;
    my $canvas = $frame->{canvas};

    my $app = wxTheApp;
    $action = $app->menu_text->{$event->GetId} if $event;
    my $thing = $app->popup_thing;
    $log->debug("thing_popup_handler: $action on " . $thing->name) if $thing;

    if ($action eq $PA_MOVE || $action eq $PA_RESIZE) {
        $app->show_vector_frame($action, $thing);
    }
    elsif ($action eq $PA_DELETE) {
        if ($thing) {
            $log->debug("delete " . $thing->name);
            $thing->do_delete;
        }
        else {
            $log->debug("delete selected things: " . join(',', @{ $app->selected_things }));
            for my $index (sort { $b <=> $a } map { $_->id } @{ $app->selected_things } ) {
                $log->debug("delete $index");
                $app->things->[$index]->do_delete;
            }
        }
    }
    elsif ($action eq $PA_CENTRE_IN_VIEW) {
        my ($offset_v, $dim_v);
        if ($thing) {
            $offset_v = [ @{ $thing->offset_v } ];
            $dim_v = $thing->dim_v;
        }
        else {
            ($offset_v, $dim_v) = $app->get_selected_extent;
        }
        my $target_v = [ @{ $app->target } ];
        for my $dim (0 .. 2) {
            $target_v->[$dim] = ($offset_v->[$dim] + $dim_v->[$dim] / 2) - $target_v->[$dim];
        }
#        $log->debug("offset, dim & target inc " . Dumper($offset_v, $dim_v, $target_v));
        $app->move_eye( @{ $target_v });
        $app->move_target( @{ $target_v });
    }
    elsif ($action eq $PA_GRID_TO_TOP) {
        my ($offset_v, $dim_v);
        if ($thing) {
            $offset_v = $thing->offset_v;
            $dim_v = $thing->dim_v;
        }
        else {
            ($offset_v, $dim_v) = $app->get_selected_extent;
        }

        for my $selected_thing ( $thing || @{ $app->selected_things } ) {
            $selected_thing->set_state($GridThing::TS_WIREFRAME) 
                unless $thing->state & $GridThing::TS_WIREFRAME;
        }

        # move grid so that it's centred on the defined area and is at the top
#        $build_grid->{offset_v}->[0] = ($offset_v->[0] + $dim_v->[0] / 2) - $build_grid->{major_count} * $build_grid->{cell_size} / 2;
#        $build_grid->{offset_v}->[2] = ($offset_v->[2] + $dim_v->[2] / 2) - $build_grid->{minor_count} * $build_grid->{cell_size} / 2;
        $build_grid->{offset_v}->[1] = $offset_v->[1] + $dim_v->[1];

        $app->refresh_grid_markings;
    }
    elsif ($action eq $PA_HIDE) {
        if ($thing) {
            $thing->set_state($GridThing::TS_HIDDEN) 
        }
        else {
            for my $thing ( @{ $app->selected_things } ) {
                $thing->set_state($GridThing::TS_HIDDEN) 
                    unless $thing->state & $GridThing::TS_HIDDEN;
            }
        }
    }
    elsif ($action eq $PA_HIDE_OTHERS) {

        # hide everything then reveal selected or current
        for my $thing ( @{ $app->things } ) {
            $thing->set_state($GridThing::TS_HIDDEN) 
        }
        if ($thing) {
            $thing->clear_state($GridThing::TS_HIDDEN) 
        }
        else {
            for my $thing ( @{ $app->selected_things } ) {
                $thing->clear_state($GridThing::TS_HIDDEN);
            }
        }
    }
    elsif ($action eq $PA_WIREFRAME) {
        if ($thing) {
            $thing->toggle_state($GridThing::TS_WIREFRAME) 
        }
        else {
            for my $thing ( @{ $app->selected_things } ) {
                $thing->toggle_state($GridThing::TS_WIREFRAME) ;
            }
        }
    }
    elsif ($action eq $PA_ADJUST) {
        if ($thing) {
            $thing->toggle_state($GridThing::TS_ADJUSTING) ;
            if ($thing->state & $GridThing::TS_ADJUSTING) {
                if ($app->adjusting_things) {
                    for my $adjusting_thing ( @{ $app->adjusting_things } ) {
                        $adjusting_thing->clear_state($GridThing::TS_ADJUSTING) ;
                    }
                }
                $app->adjusting_things([ $thing ]);
            }
            else {
                $app->adjusting_things(0);
            }
            $app->current_segment_id(0);
        }
#        else {
#            for my $thing ( @{ $app->selected_things } ) {
#                $thing->toggle_state($GridThing::TS_ADJUSTING) ;
#            }
#        }
    }
    elsif ($action eq $PA_SHOW_VERTICES) {
        if ($thing) {
            $thing->set_state($GridThing::TS_SHOW_VERTICES) 
        }
        else {
            for my $thing ( @{ $app->selected_things } ) {
                $thing->set_state($GridThing::TS_SHOW_VERTICES) 
                    unless $thing->state & $GridThing::TS_SHOW_VERTICES;
            }
        }
    }
    elsif ($action eq $PA_SHOW_ALL) {
        for my $index (0 .. $#{ $app->things }) {
            $app->things->[$index]->clear_state($GridThing::TS_HIDDEN | $GridThing::TS_WIREFRAME | $GridThing::TS_SHOW_VERTICES) 
                if $app->things->[$index]->state & ( $GridThing::TS_HIDDEN | $GridThing::TS_WIREFRAME | $GridThing::TS_SHOW_VERTICES);
        }
    }
    elsif ($action eq $PA_TOGGLE_GRID) {
        $build_grid->{hidden} = ! $build_grid->{hidden};
    }
    elsif ($action eq $PA_TOGGLE_GROUND) {
        $ground_grid->{hidden} = ! $ground_grid->{hidden};
    }
    elsif ($action eq $PA_CLEAR) {
        $log->debug("clear");
        for my $index (reverse (0 .. $#{ $app->things })) {
            $app->things->[$index]->do_delete;
        }
    }
    elsif ($action eq $PA_SELECT_ALL) {
        for my $index (0 .. $#{ $app->things }) {
            $app->things->[$index]->set_state($GridThing::TS_SELECTED) 
                unless $app->things->[$index]->state & $GridThing::TS_SELECTED;
        }
    }
    elsif ($action eq $PA_SELECT_NONE) {
        for my $index (sort { $b <=> $a } map { $_->id } @{ $app->selected_things } ) {
            $app->things->[$index]->clear_state($GridThing::TS_SELECTED);
        }
    }
    elsif ($action eq $PA_FRONT_VIEW) {
        $app->frame->{canvas}->y_rot(0);
        $app->eye( [ 0, $app->initial_eye_pos, $app->initial_eye_pos ] );
        $app->target( [ 0,0,0 ] );
    }
    else {
        $log->logdie("unknown popup action $action");
    }

}

#*******************************************************************************
sub do_vector_change { #{{{2
    my $self = wxTheApp;

    # are we changing the selected things?
    if ($self->group_reference_vector) {

        my $change_vector = [0,0,0];
        my $resizing_flag = scalar @{ $self->resizing_things };

        # yes, find out the latest change
        if (my $thing = $self->selected_things->[0]) {
            my $vector;
            if ($resizing_flag) {
                $vector = $thing->dim_v;
            }
            else {
                $vector = $thing->offset_v;
            }

            # $vector has just changed, find out how it differs from
            # the reference vector, and then update the reference vector.
            for my $dim (0 .. 2) {
                $change_vector->[$dim] = $vector->[$dim] - $self->group_reference_vector->[$dim];
                $self->group_reference_vector->[$dim] = $vector->[$dim];
            }
        }

        # apply the change to all other selected things
        for my $index (1 .. $#{ $self->selected_things }) {
            if ($resizing_flag) {
                for my $dim (0 .. 2) {
                    $self->selected_things->[$index]->dim_v->[$dim] += $change_vector->[$dim];
                }
            }
            else {
                for my $dim (0 .. 2) {
                    $self->selected_things->[$index]->offset_v->[$dim] += $change_vector->[$dim];
                }
            }
        }

        # mark all things for recompile
        for my $thing ( @{ $self->selected_things } ) {
            $thing->calculate_for_render;
        }
    }
    else {
        $self->popup_thing->calculate_for_render;
    }

    $self->refresh_grid_markings;

    return;
}

#*******************************************************************************
sub hide_vector_frame { #{{{2
    my ($self) = @_;

    if ($self->vector_frame) {
        $self->vector_frame->Hide;
    }

    return;
}

#*******************************************************************************
sub show_vector_frame { #{{{2
    my ($self, $action, $thing) = @_;

#    $log->debug("show_vector_frame: @_");

    my ($vector, $increment, $value_mode_flag);
    if ($thing) {
        $increment = $thing->grid_cell_size;
        $self->group_reference_vector(undef);
        if ($action eq $PA_MOVE) {
            $vector = $thing->offset_v;
            $self->resizing_things([ ]);
        }
        else {
            $vector = $thing->dim_v;
            $self->resizing_things([ $thing ]);
        }
        $value_mode_flag = 1;
    }
    else {

        # we are operating on the selected item(s).
        # We copy the relevant vector from the first such item and
        # track each change as it happens (ie determine the instantaneous change
        # to the vector) and apply that change to the remaining items.
        if ($thing = $self->selected_things->[0]) {
            $increment = $thing->grid_cell_size;
            if ($action eq $PA_MOVE) {
                $vector = $thing->offset_v;
                $self->resizing_things([ ]);
            }
            else {
                $vector = $thing->dim_v;
                $self->resizing_things($self->selected_things);
            }
            $self->group_reference_vector([ @{ $vector } ]);
        }
        else {
            $log->warn("no selected thing");
            return;
        }
        $value_mode_flag = 0;
    }

    if ($self->vector_frame) {
        $self->vector_frame->set_vector($vector, $increment, $value_mode_flag);
    }
    else {
        $self->vector_frame( VectorFrame->new($self->frame, -1, $vector, $increment, \&do_vector_change, $value_mode_flag));
    }

    return;
}

#*******************************************************************************
sub change_view_handler { # {{{2
    my ($frame, $event) = @_;
    my $canvas = $frame->{canvas};

    my $view_name = wxTheApp->menu_text->{$event->GetId};
    $log->debug("change_view_handler: $view_name");
    $canvas->y_rot(0);
    my $distance = wxTheApp->initial_eye_pos;
    my %xz = (
        Front => [ 0, $distance ],
        Left => [ $distance, 0, ],
        Back => [ 0, -$distance ],
        Right => [ -$distance, 0 ],
    );

    $log->logdie("bad view $view_name") unless $xz{$view_name};
    wxTheApp->eye( [ $xz{$view_name}->[0], $distance, $xz{$view_name}->[1] ] );
    wxTheApp->target( [ 0,0,0 ] );

}
#*******************************************************************************

sub change_type_handler { # {{{2
    my ($frame, $event) = @_;

    $log->debug("change_type_handler: " . $event->GetId . " = " . wxTheApp->menu_text->{$event->GetId});
    wxTheApp->thing_type(wxTheApp->menu_text->{$event->GetId});

    # find the chosen menu item; we can't find this via the event for menu events...
    my $menu_item = $frame->{type_mnu}->FindItem($event->GetId);
    if (my $bitmap = $menu_item->GetBitmap) {

        # can't change toolbar image, so remove & re-insert using bitmap from menu item
        my $pos = $frame->{toolbar}->GetToolPos($GridFrame::ID_CHANGE_THING_TYPE);
        $frame->{toolbar}->DeleteTool($GridFrame::ID_CHANGE_THING_TYPE);
        $frame->{toolbar}->InsertTool($pos,$GridFrame::ID_CHANGE_THING_TYPE, "", $bitmap,
            wxNullBitmap, wxITEM_NORMAL, "Change Thing Type", "Change Thing Type");
        $frame->{toolbar}->Realize;
    }
}

#*******************************************************************************
sub change_pattern_handler { # {{{2
    my ($frame, $event) = @_;

    $log->debug("change_pattern_handler: " . $event->GetId . " = " . wxTheApp->menu_text->{$event->GetId});
    wxTheApp->pattern(wxTheApp->menu_text->{$event->GetId});

    # find the chosen menu item; we can't find this via the event for menu events...
    my $menu_item = $frame->{pattern_mnu}->FindItem($event->GetId);
    if (my $bitmap = $menu_item->GetBitmap) {

        # can't change toolbar image, so remove & re-insert using bitmap from menu item
        my $pos = $frame->{toolbar}->GetToolPos($GridFrame::ID_CHANGE_PATTERN);
        $frame->{toolbar}->DeleteTool($GridFrame::ID_CHANGE_PATTERN);
        $frame->{toolbar}->InsertTool($pos,$GridFrame::ID_CHANGE_PATTERN, "", $bitmap,
            wxNullBitmap, wxITEM_NORMAL, "Pattern", "Change Pattern");
        $frame->{toolbar}->Realize;
    }
}

#*******************************************************************************

sub add_thing { # {{{2
    my ($self, $thing) = @_;

    # add the new thing to the list and record it as the thing under construction

    push @{ $self->things }, $thing;
    $thing->id( $#{ $self->things } );

    $self->new_thing($thing);

    $self->prop_frame->add_thing($thing);

    # new things are transparent
    $thing->set_state($GridThing::TS_TRANSPARENT);

#    $log->debug("added new thing " . Dumper($thing));

    return;
}
#*******************************************************************************

sub glow { # {{{2
    my( $self ) = shift;

    # work out glow value for highlighted cells and cache it

    my $glow = 0.5;

    # too slow...
#    my $tick = $self->tick % 31;
#    unless ($glow = $self->glow_cache->{$tick}) {
#        $glow = $self->glow_cache->{$tick} =
#            List::Util::min(abs($tick - 15),12) / 36 + 0.2;
#    }
    return $glow;
}

#*******************************************************************************

sub OnInit { # {{{2
    my( $self ) = shift;

    Wx::InitAllImageHandlers();

    my $rc = $self->SUPER::OnInit();
    $log->debug("class init $rc");

    return 1;
}

#*******************************************************************************

sub save_file { #{{{2
    my ($frame, $event, $explicit_file) = @_;
    my $self = wxTheApp
        or return;

    my $file = $explicit_file || $self->frame->{command_txt}->GetValue;

    return unless $file && $self->things;

    unless ($file =~ /\./) {
        $file .= ".yml";
    }

    # remove the rendered calculations, anything else bad...
    my $things = dclone $self->things;
    for my $thing (@{ $things }) {
        delete $thing->{render_calc}; 
    }

    DumpFile($file, $things);

    # don't log while crashing
    $log->info("File $file written ok\n");

    return $file;
}

#*******************************************************************************

sub open_file { #{{{2
    my ($frame, $event) = @_;
    my $self = wxTheApp;

    my $file = $self->frame->{command_txt}->GetValue;
    return unless $file;

    if ($file =~ /\A(.+)\.tga\z/) {

        # looking for maze_eye_0.000_9.000_17.097_target_0.000_0.000_0.000_yrot_32.000.tga
        my @tokens = split /_/, $1;
        $log->info("tokens: " . Dumper(\@tokens));

        if ($#tokens == 10) {
            $file = $tokens[0];
            $self->frame->{command_txt}->ChangeValue($file);
            $self->eye( [ @tokens[2 .. 4] ] );
            $self->target( [ @tokens[6 .. 8] ] );
            $self->frame->{canvas}->y_rot( $tokens[10] );
            $log->info("file $file, " . Dumper($self->eye, $self->target));
        }
        else {
            $log->warn("bad parse of tga filename");
            return;
        }
    }

    unless ($file =~ /\./) {
        $file .= ".yml";
    }

    unless (-f $file) {
        $log->warn("file $file not found");
        return;
    }

    # remove all existing things
    # TODO clear properties window
    $self->selected_things([]);

    $log->debug("Read from $file\n");
    $self->things( LoadFile($file) );
    for my $thing ( @{ $self->things } ) {

        if (defined $thing->texture_infotile) {
            $log->debug("tile = " . $thing->texture_infotile);
        }
        else {
            $thing->texture_infotile($thing->{tile});
            $thing->texture_infofactor(0);
            delete $thing->{tile};
        }

        # clear state manually so unselect actions don't occur
        $thing->state(0);

        # render after clearing state so things with bad dims keep TS_NO_RENDER
        $thing->calculate_for_render;

        $self->prop_frame->add_thing($thing);
    }
    $log->debug("done reading");
    $self->refresh_grid_markings;

    return;
}

#*******************************************************************************

#sub set_status_text { #{{{2
#    my ($self, $text, $index) = @_;
#
#    $self->frame->SetStatusText($text, $index);
#}

#*******************************************************************************

sub refresh_grid_markings { #{{{2
    my ($self) = @_;
    $log->debug("refresh_grid_markings");

    $build_grid->{selected_count} = [];
    for my $thing (@{ $self->selected_things }) {
        $thing->mark_on_grid;
    }

#    $self->set_status_text(sprintf("Build %dx%dx%.3f @ %.1f,%.1f,%.1f", 
#        $build_grid->{major_count}, $build_grid->{minor_count}, $build_grid->{cell_size}, @{ $build_grid->{offset_v} }), 0);

    $build_grid->{needs_compile} = 1;

    return;
}

################################################################################


package main; # {{{1

unless(caller){

    my $log = get_logger;

    $SIG{INT} = sub { $log->debug("Ouch!\n") and exit; };

    # list of options
    my @options = qw(
        man
        usage
        debug
        file=s
        new
        quiet
        no_labels
        geometry=s
    );

    GetOptions( \%options, @options ) or pod2usage(2);
    pod2usage(2) if $options{usage};
    pod2usage(1) if $options{help};
    pod2usage( -exitstatus => 0, -verbose => 2 ) if $options{man};

    # put this in %options
    $options{bin_dir} = $Bin;

    $ENV{log_appenders} = $options{quiet} ? 'file' : "file, screen";
    $ENV{log_level}     = $options{debug} ? "DEBUG" : 'INFO';
    $ENV{log_dir}       ||= '/tmp/ikmlog';

    unless (-e $ENV{log_dir}) {
        make_path($ENV{log_dir}) or die "Can't make log dir: $@";
    }

    $ENV{log_file_name} ||= 'grid';
    Log::Log4perl->init( $options{bin_dir} . '/log4perl.conf' );
    $log = get_logger();

    $log->debug("Running $0: " . Dumper(\%options));

    my $app = GridApp->new();

    if ($options{file}) {
        $app->GetTopWindow->{command_txt}->ChangeValue($options{file});
        $app->open_file();
    }

    $app->MainLoop();

}

################################################################################

__END__

=head1

TODO

Solids of revolution

Shapes - flip x & y

Change rotation centre

Specify build area from existing object(s) (possibly via a factor)

Rotate an object around a specified two-vertex axis; the combination of moving 2 objects
together and then rotating should allow us to join objects at vertices not on the grid, ie
vertices calculated from patterns, ratios, etc.

Group objects together so they can be placed as one.

Walk mode, ie look around with mouse, left click = forward, middle = back, 
right (held) = strafe (up & down as well). Height (above grid/ground?) could reduce
to 0 gradually; this would let you glide down (or up?) while turning. I think moving forward/back
would not change height at this stage. Moves of the eye would generally also be applied
to the target; you are always looking in the same direction, you just change position.
What direction you look in is changed by mouse movements in FPS style; this usually (always)
involves rotating the target around the eye somehow. Track of movements could be saved (just the
eye/target coords is enough, I think) and then replayed to get frame captures.

3d "fractal" patterns, ie cube with smaller cube at each corner, etc.

Consider the program as a display tool for our files; we can generate complicated scenes
via scripting, eg a random stair maze.

=cut
