#$Id: Window.pm 191 2009-06-24 05:16:38Z ikm $

use strict;

use Wx qw[:everything];
use Log::Log4perl qw(get_logger);
use List::MoreUtils;
use Storable qw(dclone);

my $log = get_logger;

# bitmap flags to describe the nature of a mouse event
my (
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
    $ME_MANUAL_SELECT_BUILD_HEIGHT,     # 0400
    $ME_MANUAL_IDLE,                    # 0800
    $ME_DOUBLE_CLICK,                   # 1000
) = map { 2 ** $_ } (0 .. 15);

# canvas scale & origin
my ($old_scale, $scale) = (1,1);
my ($origin_x, $origin_y) = (0,0);

package DrawCanvas; #{{{1
use base qw(Wx::Panel Class::Accessor::Fast);
use Wx qw[:everything];
use Math::Bezier;
use Math::CatmullRom;
use Data::Dumper;

__PACKAGE__->mk_accessors(qw(frame timer current_handle last_logical_x last_logical_y last_device_x last_device_y
    current_segment current_section));
################################################################################
# Constructor.
sub new {

    my( $class, $parent, $id, $pos, $size) = @_;
    $log->debug("DrawCanvas");
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = [ 200, 200 ]      unless defined $size;

    my $self = $class->SUPER::new( $parent, $id, $pos, $size);

    $self->SetCursor(wxCROSS_CURSOR);

    Wx::Event::EVT_PAINT($self, \&repaint_canvas);

    # timer for snapping
    $self->timer( Wx::Timer->new( $self ) );
    $self->timer->Start( 100, wxTIMER_ONE_SHOT );
    Wx::Event::EVT_TIMER( $self, -1,
        sub {
            my( $self, $e ) = @_;
#            $log->debug("timer");

            my $min_distance = 10;
            $self->current_handle(0);

            # while we're checking all the handles, find out if there's an in-progress thing;
            # that will skip closest line highlighting (below).
            my $in_progress = 0;

            # check handle list for closest handle
            my ($last_logical_x, $last_logical_y) = ($self->last_logical_x, $self->last_logical_y);
            my $things = $self->frame->things;
            for my $thing ( @{ $things } ) {
                if ($thing->{in_progress}) {
                    $in_progress = 1;
                    next;
                }
                for my $segment ( @{ $thing->{segments} } ) {
                    for my $handle ( @{ $segment->{handles} } ) {
                        my ($x, $y, $color, $handle_size) = @{ $handle };
                        my $distance = abs($x - $last_logical_x) + abs($y - $last_logical_y);
                        if ($distance < 10 && $min_distance > $distance) {
                            $min_distance = $distance;
                            $self->current_handle($handle);
                        }
                    }
                }
            }

            # show grid == snap to grid (after pause), but handles take precedence
            if (! $self->current_handle && $self->frame->show_grid->GetValue && defined $last_logical_x) {

                # find the grid point toward the origin
                my $grid_size = $self->frame->grid_size->GetValue;
                my ($low_grid_x, $low_grid_y) = ($last_logical_x - ($last_logical_x % $grid_size), $last_logical_y - ($last_logical_y % $grid_size));
                my @grid_points = (
                    [ $low_grid_x, $low_grid_y ],
                    [ $low_grid_x + $grid_size, $low_grid_y ],
                    [ $low_grid_x, $low_grid_y + $grid_size ],
                    [ $low_grid_x + $grid_size, $low_grid_y + $grid_size ],
                );

                $min_distance = $grid_size;
                for my $grid_point (@grid_points) {
                    my $distance = abs($grid_point->[0] - $last_logical_x) + abs($grid_point->[1] - $last_logical_y);
                    if ($distance < $min_distance) {

                        # fake a current handle so the next button event gets snapped to here
                        $self->current_handle($grid_point);

                        $min_distance = $distance;
                    }
                }
            }

            # If we found a snap position via handles or grid,
            # fake a move to this logical point to redraw any in-progress thing
            if ($self->current_handle) {
                $self->frame->mouse_event_handler(0, @{ $self->current_handle });
            }

            # if we're not in-progress on a new thing, highlight the closest line 
            # section on the current thing, no range limit. 
            # This is the line section that can be deleted.
            $self->current_segment(0);
            if (! $in_progress && (my $current_thing = $self->frame->get_current_thing)) {

                $min_distance = -1;

                for my $segment ( @{ $current_thing->{segments} } ) {

                    # the handles are easier to check as they are not in a flat list.
                    # We're looping to N - 1, eg only section start handles.
                    for my $handle_index ( 0 .. $#{ $segment->{handles} } - 1 ) {

                        # we're ignoring handle pen & size
                        my ($x1, $y1) = @{ $segment->{handles}->[$handle_index] };
                        my ($x2, $y2) = @{ $segment->{handles}->[$handle_index + 1] };

                        # average of handle distances = section distance
                        my $distance = (abs($x1 - $last_logical_x) + abs($y1 - $last_logical_y)
                            + abs($x2 - $last_logical_x) + abs($y2 - $last_logical_y)) / 2;
                        if ($min_distance < 0 || $min_distance > $distance) {
                            $min_distance = $distance;
                            $self->current_segment($segment);
                            $self->current_section($handle_index);
                        }
                    }
                }

            }

            $self->repaint_canvas;

        }
    );

    Wx::Event::EVT_LEAVE_WINDOW($self, 
        sub {
            my ($self, $e) = @_;

            # stop the timer when the cursor leaves the drawing canvas; a mouse event (ie motion) will
            # restart it on re-entry.
            $self->timer->Stop;
            $e->Skip;
        }
    );

    Wx::Event::EVT_MOUSE_EVENTS( $self,
        sub { 
            my ($self, $event) = @_;

            # restart timer on any mouse event
            $self->timer->Start(-1,wxTIMER_ONE_SHOT);

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
#            $log->debug(sprintf("event_flags = %04X", $event_flags));

            # clear current handle on motion
            unless ($event_flags) {
                $self->current_handle(0);
                $self->current_segment(0);
            }

            # use current handle if present, otherwise event location
            my @logical_point = $self->current_handle 
                ? @{ $self->current_handle }[0,1]
                : ( int(($event->GetX - $origin_x) / $old_scale),
                    int(($event->GetY - $origin_y) / $old_scale));

#            $log->debug("event at @logical_point");
            $self->frame->mouse_event_handler($event_flags, @logical_point, $event->GetX, $event->GetY);
            $self->repaint_canvas($event);

            $self->last_logical_x($logical_point[0]);
            $self->last_logical_y($logical_point[1]);
            $self->last_device_x($event->GetX);
            $self->last_device_y($event->GetY);

            # have to skip to get EVT_LEAVE_WINDOW as well
            $event->Skip;
        }
    );

    $self->frame($parent);

    return $self;
}

#*******************************************************************************
sub repaint_canvas {
    my ($self, $event) = @_;

    my $dc = Wx::PaintDC->new( $self );
    my $frame = $self->GetParent;

    $dc->Clear;

    my ($logical_x, $logical_y);
    if ((ref $event) =~ /Mouse/) {
        $logical_x = int(($event->GetX - $origin_x) / $old_scale);
        $logical_y = int(($event->GetY - $origin_y) / $old_scale);
#        $log->debug("repaint at $logical_x, $logical_y, origin = ($origin_x, $origin_y)");
    }

    if ($scale != $old_scale) {
        if ($event) {
            $origin_x = $event->GetX - $scale * $logical_x;
            $origin_y = $event->GetY - $scale * $logical_y;
#            $log->debug("scale = $scale, logical = ($logical_x, $logical_y), origin = ($origin_x, $origin_y)");
        }
        else {
            $origin_x = 0;
            $origin_y = 0;
        }
        $old_scale = $scale;
    }

    $dc->SetUserScale($scale, $scale);
    $dc->SetDeviceOrigin($origin_x, $origin_y);

    if ($frame->show_grid->GetValue) {

        # we draw the grid in logical coords; we need to know what the logical extents of the canvas are.
        my ($width, $height) = $self->GetSizeWH;
        my ($min_x, $max_x) = ( $dc->DeviceToLogicalX(0), $dc->DeviceToLogicalX($width));
        my ($min_y, $max_y) = ( $dc->DeviceToLogicalY(0), $dc->DeviceToLogicalY($height));

        # move mins & maxes to grid lines
        my $grid_size = $frame->grid_size->GetValue;
        $min_x -= $min_x % $grid_size;
        $min_y -= $min_y % $grid_size;
        $max_x += ($grid_size - ($max_x % $grid_size));
        $max_y += ($grid_size - ($max_y % $grid_size));

        $dc->SetPen(Wx::Pen->new(Wx::Colour->new(200, 200, 200), 1, wxSHORT_DASH));
        my $save_min_x = $min_x;
        while ($min_x <= $max_x) {
            $dc->DrawLine( $min_x, $min_y, $min_x, $max_y);
            $min_x += $grid_size;
        }
        while ($min_y <= $max_y) {
            $dc->DrawLine( $save_min_x, $min_y, $max_x, $min_y);
            $min_y += $grid_size;
        }
    }

    my $draw_handles = $frame->show_handles->GetValue;

    my $draw_handle_sub = sub {
        my ($x, $y, $color, $handle_size) = @_;

        $dc->SetPen($color ? $frame->pen->{$color} : wxRED_PEN);
        $handle_size ||= 10;

        $dc->DrawLine( $x, $y + $handle_size, $x + $handle_size, $y);
        $dc->DrawLine( $x, $y - $handle_size, $x + $handle_size, $y, );
        $dc->DrawLine( $x, $y - $handle_size, $x - $handle_size, $y);
        $dc->DrawLine( $x - $handle_size, $y, $x, $y + $handle_size);
        $dc->SetPen(wxBLACK_PEN);
    };

    my $current_thing = $self->frame->get_current_thing;
    my $current_segment = $self->current_segment;
    my $current_section = $self->current_section;

    for my $thing ( @{ $self->frame->things } ) {

        if ($thing->{tool} eq $DrawFrame::TT_MIRROR) {
            $dc->SetPen(wxRED_PEN);
        }
        else {
            $dc->SetPen(wxBLACK_PEN);
        }

        for my $segment ( @{ $thing->{segments} } ) {

            # connect pairs in the line_points list (x1, y1, x2, y2, etc)
            my $line_points = $segment->{line_points};
            my $last_line_point = $#{ $line_points };

            if ($current_thing && $thing == $current_thing) {
                $dc->SetPen($frame->pen->{green});
            }

            # draw lines from each start point, ie don't start a line from the final point
            my $start = 0;
            while ($start < $last_line_point - 2) {

                # this is a de-facto test for current_thing as well as we're checking the
                # segment hash ref. This means we can reset to green with the same logic.
                if ($current_segment && $segment == $current_segment && $start == $current_section * 2) {
                    $dc->SetPen($self->frame->pen->{magenta});
                }

                $dc->DrawLine( @{ $line_points }[$start .. $start + 3]);

                if ($current_segment && $segment == $current_segment && $start == $current_section * 2) {
                    $dc->SetPen($self->frame->pen->{green});
                }

                # jump by 2, 2 coords per point
                $start += 2;
            }

            # draw line point handles
            if ($draw_handles) {
                for my $handle ( @{ $segment->{handles} } ) {
                    my ($x, $y, $pen, $handle_size) = @{ $handle };
#                    if ($self->current_handle && $handle == $self->current_handle) {
#                        $pen = $self->frame->pen->{thick_green};
#                    }
                    $draw_handle_sub->($x, $y, $pen, $handle_size);
                }
            }

        }

        # draw control point handles
        if ($draw_handles) {
            for my $handle ( @{ $thing->{control_points} } ) {
                my ($x, $y, $pen, $handle_size) = @{ $handle };
#                if ($self->current_handle && $handle == $self->current_handle) {
#                    $pen = $self->frame->pen->{thick_green};
#                }
                $draw_handle_sub->($x, $y, $pen, $handle_size);
            }
        }

    }

    # highlight the current "handle", which may not be a handle but a grid intersection
    if ($self->current_handle) {
        $dc->SetPen($self->frame->pen->{green});
        $dc->SetBrush(wxGREEN_BRUSH);
        $dc->DrawCircle( @{ $self->current_handle }, 3);
    }

    return;
}

package DrawFrame; #{{{1
use base qw(Wx::MiniFrame Class::Accessor::Fast);
use Wx qw[:everything];
use Data::Dumper;
use Math::Geometry::Planar;
use Storable qw(dclone);
use YAML qw(DumpFile LoadFile);
use List::Util;

my @tools = our (
    $TT_LINE,
    $TT_BOX,
    $TT_CIRCLE,
    $TT_POLYGON,
    $TT_BEZIER,
    $TT_CATMULLROM,
    $TT_MIRROR,
    )
    = qw(Line Box Circle Polygon Bezier CatmullRom Mirror);

__PACKAGE__->mk_accessors(qw(tool nbr_points grid_size show_handles show_grid canvas things previous_down pen listbox
    dragging arc_segments first_angle));

################################################################################
# Constructor. Creates a Wx::Frame object, adds a sizer and a status bar and
# sets the window size.
sub new {
    my ($class, $parent, $title, $pos, $size) = @_;
    $log->debug("DrawFrame");

    $title  = ""                 unless defined $title;
    $pos    = [ 300, 300 ]       unless defined $pos;
    $size   = [ 250, 200 ]       unless defined $size;

    my $self = $class->SUPER::new( $parent, -1, $title, $pos, $size);

    Wx::Event::EVT_CLOSE($self, \&handle_close);

    $self->tool(Wx::Choice->new($self, -1, wxDefaultPosition, wxDefaultSize, [ @tools ]));

    $self->nbr_points(Wx::SpinCtrl->new($self, -1, 10, wxDefaultPosition, [ 60, -1 ], wxSP_ARROW_KEYS, 0, 100));
    $self->nbr_points->SetToolTip('Points in circles & curves');
    main::assign_event_handler($self->nbr_points, 'EVT_SPINCTRL',
        sub {
            if (my $in_progress_thing = $self->get_in_progress_thing) {
                $self->calculate_thing($in_progress_thing);
                $self->canvas->repaint_canvas;
            }
        });

    $self->grid_size(Wx::SpinCtrl->new($self, -1, 30, wxDefaultPosition, [ 60, -1 ], wxSP_ARROW_KEYS, 4, 10000));
    $self->grid_size->SetToolTip('Grid size');
    main::assign_event_handler($self->grid_size, 'EVT_SPINCTRL', sub { $self->canvas->repaint_canvas; });

    $self->arc_segments(Wx::SpinCtrl->new($self, -1, 0, wxDefaultPosition, [ 60, -1 ], wxSP_ARROW_KEYS, 0, 100));
    $self->arc_segments->SetToolTip('Arc Segments');

    $self->first_angle(Wx::SpinCtrl->new($self, -1, 0, wxDefaultPosition, [ 60, -1 ], wxSP_ARROW_KEYS, 0, 360));
    $self->first_angle->SetToolTip('Angle of first Arc segment');

    $self->show_handles(Wx::CheckBox->new($self, -1, 'Handles'));
    $self->show_handles->SetValue(1);
    main::assign_event_handler($self->show_handles, 'EVT_CHECKBOX', sub { $self->canvas->repaint_canvas; });

    $self->show_grid(Wx::CheckBox->new($self, -1, 'Grid'));
    $self->show_grid->SetValue(0);
    main::assign_event_handler($self->show_grid, 'EVT_CHECKBOX', sub { $self->canvas->repaint_canvas; });

    my $clear_btn = Wx::Button->new($self, -1, 'Clear', wxDefaultPosition, [ 60, -1 ]);
    main::assign_event_handler($clear_btn, 'EVT_BUTTON',
        sub {
            $self->things([]);
            $self->listbox->Clear;
            $self->canvas->repaint_canvas;
        });

    my $reset_scale_btn = Wx::Button->new($self, -1, 'Reset', wxDefaultPosition, [ 60, -1 ]);
    main::assign_event_handler($reset_scale_btn, 'EVT_BUTTON',
        sub {
            $old_scale = 2;
            $scale = 1;
            $self->listbox->SetSelection(-1);
            $self->canvas->repaint_canvas;
        });

    my $save_btn = Wx::Button->new($self, -1, 'Save', wxDefaultPosition, [ 60, -1 ]);
    main::assign_event_handler($save_btn, 'EVT_BUTTON', \&save_to_file);

    my $open_btn = Wx::Button->new($self, -1, 'Open', wxDefaultPosition, [ 60, -1 ]);
    main::assign_event_handler($open_btn, 'EVT_BUTTON', \&open_from_file);

    $self->listbox(Wx::ListBox->new($self, -1,));
    main::assign_event_handler($self->listbox, 'EVT_CHAR',
        sub {
            my ($listbox, $event) = @_;

            if ($event->GetKeyCode == WXK_DELETE) {
                $self->delete_current_thing;
            }
        });
    main::assign_event_handler($self->listbox, 'EVT_LISTBOX',
        sub {
            $self->canvas->repaint_canvas;
        });

    $self->canvas(DrawCanvas->new($self, -1));

    $self->SetSizer(my $sizer = Wx::BoxSizer->new(wxHORIZONTAL));
    my $tool_sizer = Wx::BoxSizer->new(wxVERTICAL);

    $tool_sizer->Add($self->tool, 0, wxEXPAND, 0);

    my $line_sizer = Wx::BoxSizer->new(wxHORIZONTAL);
    $line_sizer->Add($self->nbr_points, 0, wxEXPAND, 0);
    $line_sizer->Add($self->grid_size, 0, wxEXPAND, 0);
    $tool_sizer->Add($line_sizer, 0, wxEXPAND, 0);

    $line_sizer = Wx::BoxSizer->new(wxHORIZONTAL);
    $line_sizer->Add($self->arc_segments, 0, wxEXPAND, 0);
    $line_sizer->Add($self->first_angle, 0, wxEXPAND, 0);
    $tool_sizer->Add($line_sizer, 0, wxEXPAND, 0);

    $line_sizer = Wx::BoxSizer->new(wxHORIZONTAL);
    $line_sizer->Add($self->show_handles, 0, 0, 0);
    $line_sizer->Add($self->show_grid, 0, 0, 0);
    $tool_sizer->Add($line_sizer, 0, wxEXPAND, 0);

    $line_sizer = Wx::BoxSizer->new(wxHORIZONTAL);
    $line_sizer->Add($clear_btn, 0, wxEXPAND, 0);
    $line_sizer->Add($reset_scale_btn, 0, wxEXPAND, 0);
    $tool_sizer->Add($line_sizer, 0, wxEXPAND, 0);

    $tool_sizer->Add($self->listbox, 1, wxEXPAND, 0);

    $line_sizer = Wx::BoxSizer->new(wxHORIZONTAL);
    $line_sizer->Add($save_btn, 0, wxEXPAND, 0);
    $line_sizer->Add($open_btn, 0, wxEXPAND, 0);
    $tool_sizer->Add($line_sizer, 0, wxEXPAND, 0);

    $sizer->Add($tool_sizer, 0, wxEXPAND, 0 );
    $sizer->Add($self->canvas, 1, wxEXPAND, 0 );

    $self->things([]);

    $self->pen({
        blue => Wx::Pen->new('blue', 2, wxSOLID),
        thick_green => Wx::Pen->new('green', 2, wxSOLID),
        green => Wx::Pen->new('green', 1, wxSOLID),
        magenta => Wx::Pen->new('magenta', 1, wxSOLID),
    });

    return $self;
}

################################################################################
# Find the currently selected thing, if any
sub get_current_thing {
    my ($self) = @_;

    my $index = $self->listbox->GetSelection;
    return $index >= 0 ? $self->things->[$index] : undef;
}

################################################################################
# Find the in-progress thing, if any
sub get_in_progress_thing {
    my ($self) = @_;

    my $things = $self->things;
    my $top_thing_index = $#{ $things };
    if ($top_thing_index >= 0 && $things->[$top_thing_index]->{in_progress}) {
        return $things->[$top_thing_index];
    }
    
    return;
}

################################################################################
# Delete the currently selected thing
sub delete_current_thing {
    my ($self) = @_;

    my $index = $self->listbox->GetSelection;
    if ($index > -1) {
        splice @{ $self->things }, $index, 1;
        $self->listbox->Delete($index);
    }
}

################################################################################
# Create a new thing
sub create_thing {
    my ($self, $new_thing) = @_;

    $new_thing->{in_progress} = 1;

    $log->debug("new thing:" . Dumper($new_thing));

    push @{ $self->things }, $new_thing;

    $self->listbox->Append($new_thing->{tool});

    return $new_thing;
}

################################################################################
# Calculate line points, handles for a thing
sub calculate_thing {
    my ($self, $thing) = @_;

    # everything is a sequence of lines; build a flat list of coords, ie x1, y1, x2, y2, ...
    my @points;

    # this can be 0 in which case each tool defines an appropriate default
    my $nbr_points = (defined $thing->{nbr_points} ? $thing->{nbr_points} : $self->nbr_points->GetValue);

    if ($thing->{tool} eq $DrawFrame::TT_LINE) {
        push @points, @{ $thing->{start} }, @{ $thing->{end} };
    }
    elsif ($thing->{tool} eq $DrawFrame::TT_MIRROR) {
        push @points, @{ $thing->{start} }, @{ $thing->{end} };
    }
    elsif ($thing->{tool} eq $DrawFrame::TT_BOX) {
        push @points, @{ $thing->{start} },
            $thing->{start}->[0], $thing->{end}->[1],
            @{ $thing->{end} },
            $thing->{end}->[0], $thing->{start}->[1],
            @{ $thing->{start} };
    }
    elsif ($thing->{tool} eq $DrawFrame::TT_CIRCLE) {
        $nbr_points ||= 12;

        # use our 3d circle function and strip out one dim, in this case Z
        my @three_d_points = main::find_stretched_ring_points(
            [ @{ $thing->{start} }, 0 ],
            2, # z axis
            $self->first_angle->GetValue, # start
            abs($thing->{start}->[0] - $thing->{end}->[0] ), abs($thing->{start}->[1] - $thing->{end}->[1] ),
            360 / $nbr_points,
            $self->arc_segments->GetValue || ($nbr_points + 1));
        map { push @points, $_->[0], $_->[1]; } @three_d_points;
    }
    elsif ($thing->{tool} eq $DrawFrame::TT_POLYGON) {
        if (scalar @{ $thing->{points} } >= 4) {
            push @points, @{ $thing->{points} };
        }
    }
    elsif ($thing->{tool} =~ /$DrawFrame::TT_BEZIER|$DrawFrame::TT_CATMULLROM/o) {
        if (scalar @{ $thing->{points} } >= 4) {
            $nbr_points ||= (4 * scalar @{ $thing->{points} });
            if ($thing->{tool} eq $DrawFrame::TT_BEZIER) {
                my $bezier = Math::Bezier->new( $thing->{points} );
                if ($nbr_points == 1) {
                    $nbr_points = 2;
                    $self->nbr_points->SetValue($nbr_points);
                }
                @points = $bezier->curve($nbr_points);
            }
            else {
                my $last = $#{ $thing->{points} };

                # automatically add the first and last points again, so the curve touches all the
                # drawn points
                my $catmull = Math::CatmullRom->new(
                    @{ $thing->{points} }[0,1], @{ $thing->{points} }, @{ $thing->{points} }[$last-1,$last] );
                @points = $catmull->curve($nbr_points);
            }
        }
    }
    else {
        $log->logdie("bad tool: $thing->{tool}");
    }

    # create things with a list of segments (initially just 1 exists), so we can split into multiple segments later by deleting
    # lines.
    $thing->{segments} = [
        {
            line_points => \@points,
            handles => [],
        }
    ];

    # line point handles
    for my $point (0 .. ($#points - 1) / 2) {
        push @{ $thing->{segments}->[0]->{handles} }, [ $points[$point * 2], $points[$point * 2 + 1] ];
    }

    # control point handles for curves; part of the thing, not a segment.
    # When we introduce control point editing, that will regenerate the initial single segment, 
    # deleting any fragments that may have been created.
    $thing->{control_points} = [];
    if ($thing->{tool} =~ /$DrawFrame::TT_BEZIER|$DrawFrame::TT_CATMULLROM/o) {
        for my $point (0 .. ($#{ $thing->{points} } - 1) / 2) {
            push @{ $thing->{control_points} },
#                [ $thing->{points}->[$point * 2], $thing->{points}->[$point * 2 + 1], ];
                [ $thing->{points}->[$point * 2], $thing->{points}->[$point * 2 + 1], 'blue', 7];
        }
    }

    return;
}
#*******************************************************************************
sub mouse_event_handler {
    my ($self, $event_flags, $logical_x, $logical_y, $device_x, $device_y) = @_;

    my $tool = $self->tool->GetString($self->tool->GetSelection);
    my $previous_down = $self->previous_down;
    my $things = $self->things;
    my $top_thing_index = $#{ $things };
    my $updated_thing;
    my $in_progress_thing;

    if ($top_thing_index >= 0 && $things->[$top_thing_index]->{in_progress}) {
        $in_progress_thing = $things->[$top_thing_index];
    }

    if ($tool =~ /$TT_LINE|$TT_BOX|$TT_CIRCLE|$TT_MIRROR/o) {
        if ($previous_down) {

            # cope with shifted movements, which make straight lines, squares, etc 
            if ($event_flags == $ME_SHIFT_IS_DOWN && $in_progress_thing) {
                my $x_move = abs($in_progress_thing->{start}->[0] - $logical_x);
                my $y_move = abs($in_progress_thing->{start}->[1] - $logical_y);
                if ($tool =~ /$TT_LINE|$TT_MIRROR/o) {

                    # straight line in the larger diff direction
                    if ($x_move > $y_move) {
                        $logical_y = $in_progress_thing->{start}->[1];
                    }
                    else {
                        $logical_x = $in_progress_thing->{start}->[0];
                    }
                }
                elsif ($tool =~ /$TT_BOX|$TT_CIRCLE/o) {

                    # make square boxes, circular ellipses...

                    # square in the larger diff direction
                    if ($x_move > $y_move) {

                        # preserve the sign of the current y move
                        $logical_y = $in_progress_thing->{start}->[1] + 
                            $x_move * ($logical_y > $in_progress_thing->{start}->[1] ? 1 : -1);
                    }
                    else {
                        # preserve the sign of the current y move
                        $logical_x = $in_progress_thing->{start}->[0] + 
                            $y_move * ($logical_x > $in_progress_thing->{start}->[0] ? 1 : -1);
                    }
                }
                $event_flags = 0;
            }

            if ($event_flags == 0) {

                # motion; is there an in-progress thing?
                if ($in_progress_thing) {

                    # yes, update the end position to here
                    $in_progress_thing->{end} = [ $logical_x, $logical_y ];
                }
                else {

                    # no, add a line/box/circle from previous to current
                    $self->create_thing({ 
                        tool => $tool,
                        start => $previous_down, 
                        end => [ $logical_x, $logical_y ],
                    });

                }

                # the thing at the top of the list now (which may not be at $top_thing_index)
                # has been updated
                $updated_thing = $things->[ $#{ $things } ];

            }

            if ($event_flags & $ME_BUTTON_UP && $event_flags & $ME_LEFT_BUTTON) {

                # if there is an in-progress thing, make it permanent
                if ($in_progress_thing) {
                    if ($tool eq $TT_MIRROR) {

                        # mirrors don't get created, they just mirror all existing objects

                        # we can delete the mirror first of all so it doesn't get mirrored;
                        # we still have the $in_progress_thing reference to it.
                        $self->listbox->SetSelection($self->listbox->GetCount - 1);
                        $self->delete_current_thing;

                        my $polygon = Math::Geometry::Planar->new;
                        my $axis = [ $in_progress_thing->{start}, $in_progress_thing->{end} ];
                        my @new_things;

                        for my $thing ( @{ $self->things } ) {
                            my $new_thing = dclone $thing;

                            # mirror the starting settings so we can edit mirrored things too
#                            if ($new_thing->{tool} =~ /$TT_LINE|$TT_BOX|$TT_CIRCLE/o) {
#                                $polygon->points( [ $new_thing->{start}, $new_thing->{end} ] );
#                                $polygon->mirror( $axis );
#                                $new_thing->{start} = $polygon->{points}->[0];
#                                $new_thing->{end} = $polygon->{points}->[1];
#                            }
#                            elsif ($new_thing->{tool} =~ /$TT_POLYGON|$TT_BEZIER|$TT_CATMULLROM/o) {
#
#                                # we have to convert our flat points list into a list of lists
#                                my @point_list = map { [ $new_thing->{points}->[$_ * 2], $new_thing->{points}->[$_ * 2 + 1] ] }
#                                    (0 .. (scalar @{ $new_thing->{points} } / 2 - 1));
#                                $polygon->points( \@point_list );
#                                $polygon->mirror( $axis );
#
#                                # re-flatten list
#                                $new_thing->{points} = [ map { $_->[0], $_->[1] } ] @{ $polygon->{points} });
#                            }

                            # for all things, mirror the existing segments, so we preserve any line removal
                            for my $segment ( @{ $new_thing->{segments} } ) {

                                # we have to convert our flat points list into a list of lists
                                my @point_list = map { [ $segment->{line_points}->[$_ * 2], $segment->{line_points}->[$_ * 2 + 1] ] }
                                    (0 .. (scalar @{ $segment->{line_points} } / 2 - 1));
                                $polygon->points( \@point_list );
                                $polygon->mirror( $axis );

                                # re-flatten list
                                $segment->{line_points} = [ map { $_->[0], $_->[1] } @{ $polygon->{points} } ];

                                # handles is already a structured list but it contains extra info
                                # TODO which we're currently ignoring
                                $polygon->points( $segment->{handles} );
                                $polygon->mirror( $axis );
                                $segment->{handles} = dclone $polygon->points;

                            }

                            # don't add while we're still mirroring
                            push @new_things, $new_thing;
                        }

                        # ok, add all the mirrored things
                        for my $new_thing ( @new_things ) {
                            $self->create_thing($new_thing);
                            delete $new_thing->{in_progress};
                        }

                    }
                    else {

                        delete $in_progress_thing->{in_progress};
                        $in_progress_thing->{nbr_points} = $self->nbr_points->GetValue;
                    }
                }

                $self->previous_down(0);
            }

        }
        else {
            if ($event_flags & $ME_BUTTON_DOWN && $event_flags & $ME_LEFT_BUTTON) {
                $log->debug("down at $logical_x, $logical_y");
                $self->previous_down( [ $logical_x, $logical_y ] );
            }
        }
    }
    elsif ($tool =~ /$TT_POLYGON|$TT_BEZIER|$TT_CATMULLROM/o) {

        # for point lists, all we care about are left-down clicks to add points and right clicks to stop
        if ($event_flags & $ME_BUTTON_DOWN && $event_flags & $ME_LEFT_BUTTON) {

            unless ($in_progress_thing) {

                # no, add a point list starting here
                $self->create_thing({ 
                    tool => $tool,
                    points => [ $logical_x, $logical_y ],
                });

            }

            # the thing at the top of the list now (which may not be at $top_thing_index)
            # has been updated
            $updated_thing = $things->[ $#{ $things } ];

            # note that for clicks after the first one, we do nothing but set this flag, so
            # until the mouse moves, no more points are added.
            $updated_thing->{just_clicked} = 1;
        }
        elsif ($event_flags == 0) {
            if ($in_progress_thing) {

                # for the point list objects, render to the current location. We create a new point 
                # on the first move after a click and thereafter update that point, which will be
                # fixed as the next point after the next click and so forth.
                if ($in_progress_thing->{just_clicked}) {

                    # add a new point here
                    push @{ $in_progress_thing->{points} }, $logical_x, $logical_y;
                    delete $in_progress_thing->{just_clicked};
                }
                else {

                    # we've already added the new point, just update it with the new location
                    splice @{ $in_progress_thing->{points} }, -2, 2, $logical_x, $logical_y;
                }
                $updated_thing = $in_progress_thing;
            }
        }
        elsif ($event_flags & $ME_BUTTON_DOWN && $event_flags & $ME_RIGHT_BUTTON) {
            if ($in_progress_thing) {
                delete $in_progress_thing->{in_progress};
                delete $in_progress_thing->{just_clicked};

                # lock the current nbr points setting for curves
                $in_progress_thing->{nbr_points} = $self->nbr_points->GetValue
                    unless $tool eq $DrawFrame::TT_POLYGON;
            }
        }
    }

    # check for line section deletion
    my $current_segment = $self->canvas->current_segment;
    if ($event_flags & $ME_BUTTON_DOWN && $event_flags & $ME_RIGHT_BUTTON && $current_segment) {
        my $current_thing = $self->get_current_thing;
        my $current_section = $self->canvas->current_section;

        for my $segment_index ( 0 .. $#{ $current_thing->{segments} } ) {
            my $segment = $current_thing->{segments}->[$segment_index];
            if ($segment == $current_segment) {

                if (scalar @{ $segment->{handles} } == 2) {

                    # only two handles ie one line section, we can delete the segment
                    # or maybe the thing
                    if (scalar @{ $current_thing->{segments} } == 1) {
                        
                        # delete the thing
                        $self->delete_current_thing;
                    }
                    else {

                        # delete the segment
                        splice @{ $current_thing->{segments} }, $segment_index, 1;
                    }
                }
                else {

                    if ($current_section == 0) {

                        # delete first point in segment
                        shift @{ $segment->{handles} };
                        shift @{ $segment->{line_points} };
                        shift @{ $segment->{line_points} };
                    }
                    elsif ($current_section == $#{ $segment->{handles} } - 1) {

                        # delete last point in segment
                        pop @{ $segment->{handles} };
                        pop @{ $segment->{line_points} };
                        pop @{ $segment->{line_points} };
                    }
                    else {

                        $log->debug("split at $current_section: " . Dumper($current_thing));

                        # split segment into two between section and section + 1
                        my @line_points = splice @{ $segment->{line_points} }, ($current_section + 1) * 2, ($#{ $segment->{handles} } - $current_section) * 2;
                        my @handles = splice @{ $segment->{handles} }, $current_section + 1, $#{ $segment->{handles} } - $current_section;

                        push @{ $current_thing->{segments} },
                            {
                                handles => \@handles,
                                line_points => \@line_points,
                            };

                        $log->debug("now " . Dumper($current_thing));
                    }
                }

                last;
            }
        }
    }

    if ($updated_thing) {
        $self->calculate_thing($updated_thing);
    }

    if ($event_flags & $ME_WHEEL_FORWARD) {
        $scale *= 2;

        # reset to 1 when coming back up from fractional end of range
        $scale = 1 if ($scale > 0.9 && $scale < 1.1);
    }
    elsif ($event_flags & $ME_WHEEL_BACK) {
        $scale /= 2;
    }

    # middle button down/up toggles dragging flag
    if ($event_flags & $ME_MIDDLE_BUTTON) {
        $self->dragging($event_flags & $ME_BUTTON_DOWN);
    }

    # motion while dragging moves origin
    if (defined $device_x && $self->dragging && $event_flags == 0) {
        $origin_x -= $self->canvas->last_device_x - $device_x;
        $origin_y -= $self->canvas->last_device_y - $device_y;
    }

    return;
}

#*******************************************************************************
sub handle_close {
    my ($self, $event) = @_;

    if ($event->CanVeto) {
        $event->Veto();
        $self->Hide;
    }
    else {
        $self->Destroy;
    }

}

#*******************************************************************************
sub save_to_file { #{{{2
    my ($self, $event) = @_;

    return unless @{ $self->things };

    # scan the things to find a single line of connected points, which is what we 
    # actually render in grid.

    # things have lists of line segments, which are lists of connected points by definition;
    # what we're doing is finding out how to fit those segments together. 
    # We take the first thing's first segment as a starting point and look for another segment
    # that starts where it finishes, and so on. When we arrive back at the first segment, 
    # we've closed the loop, and hopefully all segments have been included. We don't 
    # make anything illegal (it may be a work in progress) but we warn if the loop doesn't close
    # or if some segments are not included. We DON'T warn (and never will) if the line crosses itself.
    # If two segments start on the same point, expect (and deserve) problems.

    # we may need to try this more than once, adjusting the tolerance on the fly to prevent clashes
    # and cope with inexact point placement.
    my $tolerance = 0.01;
    my $tolerance_change_required = 1;

    # this will end up with a flat list of points; if the loop closes, the return to the starting point is 
    # included in the list (since the list may not close).
    my @path;

    # count the segments so we know when we're done
    my $nbr_segments = 0;
    for my $thing ( @{ $self->things } ) {
        $nbr_segments += scalar @{ $thing->{segments} };
    }

    FIND_PATH:
    while (defined $tolerance_change_required) {

        # we start with tolerance_change_required set to 1 so we enter the loop;
        # we'll then clear it so we know if it gets set and thus we have to go around again,
        # after applying the required change.
        $tolerance *= $tolerance_change_required;
        $tolerance_change_required = undef;

        if ($tolerance > 10 || $tolerance < 1e-10) {
            $log->warn("tolerance out of range: $tolerance");
            @path = ();
            last;
        }

        my $current_segment = $self->things->[0]->{segments}->[0];
        my %done_segment = ();
        my $closed_loop = 0;
        my $reverse_segment = 0;

        @path = ();

        $log->debug("start to find path, tolerance = $tolerance");

        while (! $closed_loop && ! defined $tolerance_change_required && scalar keys %done_segment < $nbr_segments && $current_segment) {

            # adding points is not trivial; may have to reverse if we matched on a segment end,
            # and want to drop the first pair for all segments after the first.

            # make a list of point indexes (not point coord indexes)
            my @points = (0 .. (scalar @{ $current_segment->{line_points} } / 2) - 1);
            my @end_current;
            if ($reverse_segment) {
                @points = reverse @points;

                # we're reversing, so match to start of this segment
                @end_current = @{ $current_segment->{line_points} }[0,1];
            }
            else {

                # next segment should match end of this one
                @end_current = @{ $current_segment->{line_points} }[ $#{ $current_segment->{line_points} } - 1, $#{ $current_segment->{line_points} } ] ;
            }

            # drop the first point to avoid dups
            shift @points if scalar keys %done_segment;

            # restore to point coord indexes, ie what line_points needs
            @points = map { $_ * 2, $_ * 2 + 1 } @points;

            # add this segment to the path
            $log->debug("add points @points");
            push @path, @{ $current_segment->{line_points} }[@points];
            $done_segment{$current_segment} = 1;

            # special case; if there's only one segment in the shape, don't bother trying to match end to start.
            # This lets us skip the current segment below
            # We'll get a warning for non-closure.
            last if $nbr_segments == 1;

            # check for a segment starting where this one finishes
            my $next_segment = undef;
            for my $thing ( @{ $self->things } ) {

                $log->debug("try thing " . $thing->{tool});

                for my $segment ( @{ $thing->{segments} } ) {

                    # we don't want to join a segment to itself (one-segment shapes are handled above)
                    next if $current_segment == $segment;

                    $log->debug("try segment");

                    my @segment_points = @{ $segment->{line_points} };
                    my @start_point = @segment_points[0,1];
                    my @end_point = @segment_points[$#segment_points-1,$#segment_points];
                    my $match;
                    if ($match = ((abs($start_point[0] - $end_current[0]) + abs($start_point[1] - $end_current[1])) < $tolerance)) {
                        $log->debug("matched next start");
                        $reverse_segment = 0;
                    }
                    elsif ($match = ((abs($end_point[0] - $end_current[0]) + abs($end_point[1] - $end_current[1])) < $tolerance)) {
                        $log->debug("matched next end, add reversed");
                        $reverse_segment = 1;
                    }

                    if ($match) {
                        
                        # note that we don't stop looking for starts because we found a match, as we
                        # want to detect clashing starts, and maybe adjust tolerance to cope.
                        if (defined $next_segment) {

                            $log->info("clashing segment start found");

                            # clash; are the segment starts identical? if so, adjusting tolerance 
                            # won't help us.
                            if ($next_segment->{line_points}->[0] == $segment->{line_points}->[0]
                                && $next_segment->{line_points}->[1] == $segment->{line_points}->[1])
                            {
                                $log->warn("two segments have identical start points");
                                @path = ();
                                last FIND_PATH;
                            }
                            else {

                                # the start points aren't actually identical, so tightening tolerance might help us
                                if ($tolerance <= 0.01) {
                                    $tolerance_change_required = 0.5;
                                    $log->debug("tightening tolerance to avoid clash");
                                }
                                else {
                                    $log->warn("we loosened tolerance and now have clashes, stop");
                                    @path = ();
                                    last FIND_PATH;
                                }
                            }
                        }

                        # this is a match
                        $next_segment = $segment;

                        # have we closed the loop?
                        if ($done_segment{$segment}) {
                            $log->debug("closed loop");
                            $closed_loop = 1;
                        }

                        # We've found the next starting point; make sure the current closing point is exactly equal to that
                        # point, not just within tolerance (?)

                    }
                }
            }

            $current_segment = $next_segment;

            unless ($current_segment) {

                # if we don't find a match, we can loosen the tolerance in case we specified a point manually.
                # We don't do this if already tightened the tolerance.
                if ($tolerance >= 0.01 ) {
                    $log->debug("didn't close loop, loosening tolerance");
                    $tolerance_change_required = 2;
                }
                else {

                    # we're boned; we tightened tolerance to avoid a clash and now we're
                    # missing a link.
                    $log->warn("we tightened tolerance and now are missing links, stop");
                }
            }
        }

        if (! defined $tolerance_change_required && ! $closed_loop) {
            $log->warn("path is not closed");
        }

    }

    $log->debug("path = " . Dumper(\@path));

    my (@min, @max);
    if (@path) {

        # we now want to adjust the path values so they range from 0 to 1. We keep the aspect ratio
        # of the path by setting the conversion ratio according to the larger dimension.
        for my $point (0 .. (scalar @path) / 2 - 1) {
            for my $dim (0, 1) {
                $min[$dim] = $path[$point * 2 + $dim] if ! defined $min[$dim] || $path[$point * 2 + $dim] < $min[$dim];
                $max[$dim] = $path[$point * 2 + $dim] if ! defined $max[$dim] || $path[$point * 2 + $dim] > $max[$dim];
            }
        }

        # convert maxima to span
        map { $max[$_] -= $min[$_] } (0,1);

        # find larger dimension
        my $max_overall = List::Util::max(@max);

        my @test;

        for my $point (0 .. (scalar @path) / 2 - 1) {
            for my $dim (0, 1) {
                
                # convert value to 0 to max dim
                $path[$point * 2 + $dim] -= $min[$dim];

                $test[$point]->[$dim] = $path[$point * 2 + $dim];

                # express value as ratio of max overall
                $path[$point * 2 + $dim] /= $max_overall;

            }
        }

        # convert @max to scaled range; we store this with the shape
        map { $max[$_] /= $max_overall } (0,1);

        pop @test;
        $log->debug("test for triangles " . Dumper(\@test));
        my $polygon = Math::Geometry::Planar->new;
        eval { 
            $polygon->points(\@test);
        };
        if ($@) {
            $log->warn("error making polygon: $@");
        }
        else {
            my @triangles;
            eval {
                @triangles = $polygon->triangulate;
            };
            if ($@) {
                $log->warn("error triangulating: $@");
            }
            else {
                $log->debug("triangles: " . Dumper(\@triangles));
            }
        }
    }

    my $dialog = Wx::FileDialog->new( $self,
        'Save drawing to file',
        '',
        '',
        '2D files (*.2d)|*.2d',
        wxFD_SAVE);

    return 0 if $dialog->ShowModal == wxID_CANCEL;
    return 0 unless my $file = ($dialog->GetPaths)[0];

    $file .= ".2d" unless $file =~ /\./;

    DumpFile($file, 
        {
            things => $self->things,
            origin_x => $origin_x,
            origin_y => $origin_y,
            scale => $scale,
            path => \@path,

            # max works better in grid.pl when expressed in x & y terms, since we may
            # be mapping onto a different axis and we have conceptual means for mapping "x" & "y"
            # onto an axis (ie axis_dim), but not 0 & 1.
            max => {
                x => $max[0],
                y => $max[1],
            },
        });

    $log->debug("File $file written ok\n");

    return;
}

#*******************************************************************************

sub open_from_file { #{{{2
    my ($self, $event) = @_;

    my $dialog = Wx::FileDialog->new( $self,
        'Open drawing from file',
        '',
        '',
        '2D files (*.2d)|*.2d',
        wxFD_OPEN);

    return 0 if $dialog->ShowModal == wxID_CANCEL;
    return 0 unless my $file = ($dialog->GetPaths)[0];

    my $drawing = LoadFile($file);
    $self->things( $drawing->{things} );
    $origin_x = $drawing->{origin_x};
    $origin_y = $drawing->{origin_y};
    $scale = $drawing->{scale};

    $self->listbox->Clear;
    for my $thing ( @{ $self->things } ) {
        $self->listbox->Append($thing->{tool});
    }

    $self->canvas->repaint_canvas;

    return;
}

1;

# todo...
