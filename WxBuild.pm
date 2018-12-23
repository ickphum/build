use strict;
package WxBuild;
use Moose;
use Log::Log4perl qw(get_logger);
use Storable qw(dclone);
use Wx qw(:bitmap :color :image wxTheApp);

my $log = get_logger;

use WxBuildModel;
use WxBuildView;

# cursor states; these are bitmap flags
our ( $CS_EYE, $CS_LIGHT, $CS_TARGET,) = (1, 2, 4);

# object types
our ($OT_LINE, $OT_QUAD, $OT_CUBOID, $OT_POLYGON, $OT_PRISM, $OT_RAMP, $OT_STAIRS, )
    = (1 .. 100);

has 'frame' => (
    isa => 'WxBuildWin',
    is => 'ro',
);

has 'views' => (
    isa => 'ArrayRef[WxBuildView]',
    is => 'ro',
    default => sub {[]},
);

has 'current_view' => (
    isa => 'WxBuildView',
    is => 'rw',
);

has 'view' => (
    isa => 'HashRef',
    is => 'ro',
    default => sub {{}},
);

has model => (
    is => 'rw',
    isa => 'WxBuildModel',
);

has 'drag_to_show_flag' => (
    isa => 'Bool',
    is => 'rw',
    default => 0,
);

has 'listed_thing_indices' => (
    isa => 'ArrayRef',
    is => 'rw',
    default => sub {[]},
);

has 'cursor' => (
    isa => 'WxBuildVertex',
    is => 'ro',
    default => sub { WxBuildVertex->new(), },
);

has 'cursor_state' => (
    isa => 'Int',
    is => 'rw',
    default => 0,
);

has 'new_vertex' => (
    isa => 'HashRef',
    is => 'rw',
    default => sub {{}},
);

has 'new_vertices' => (
    isa => 'ArrayRef',
    is => 'rw',
    default => sub {[]},
);

has 'new_thing' => (
    is => 'rw',
);

has 'system_thing' => (
    isa => 'HashRef',
    is => 'rw',
);

has 'cursor_size' => (
    isa => 'Num',
    is => 'rw',
    default => 100,
);

has 'capturing' => (
    isa => 'Str',
    is => 'rw',
    default => 0,
);

has 'capture_index' => ( 
    isa => 'Int', 
    is => 'rw',
    default => 0,
);

after 'model' => sub {
    my ($self, $model) = @_;
    if ($model) {
        $self->refresh_object_list;
        $self->update_listed_thing_indices;

        # refresh the views according to the saved info with the model

        # update the ortho views
        for my $ortho_view ( values %{ $model->view_setting->{ortho} } ) {

            my $view = $self->view->{$ortho_view->{name}};
            unless ($view) {
                $log->debug("no view for saved view $ortho_view->{name}");
                next;
            }

            for my $key ( qw(size x_centre y_centre precision) ) {
                $log->debug("view $ortho_view->{name}, set $key");
                $view->$key($ortho_view->{$key});
            }

            $view->model($model);

        }

        # perspective settings
        my $p_settings = $model->view_setting->{perspective};
        for my $key ( keys %{ $p_settings->{perspective} } ) {
            $log->debug("set perspective key $key");
            $self->view->{perspective}->perspective->{$key} = $model->view_setting->{perspective}->{perspective}->{$key};
            # TODO handling for vertex and setting hashes from saved perspective view
        }
        $self->view->{perspective}->model($model);

    }
};

after 'cursor_size' => sub {
    my ($self, $cursor_size) = @_;
    if ($cursor_size) {

        my @cursor_coords = $self->cursor->vertex; 
#        $self->adjust_cursor_lines(\@cursor_coords);

        $self->refresh_all_views;
    }
};

after 'current_view' => sub {
    my ($self, $current_view) = @_;
    if ($current_view) {
        
        # this is derivable, I think...
        my %default_skips = (
            XY => {
                0 => [ qw(XZ YZ) ],
                1 => [ qw(XZ YZ) ],
                2 => [ qw(XY) ],
            },
            XZ => {
                0 => [ qw(XY YZ) ],
                1 => [ qw(XZ) ],
                2 => [ qw(XY YZ) ],
            },
            YZ => {
                0 => [ qw(YZ) ],
                1 => [ qw(XY XZ) ],
                2 => [ qw(XY XZ) ],
            },
        );

        if (my $skip = dclone $default_skips{$current_view->name}) {

            my $app = wxTheApp->{app};

            # add skips for in-progress vertex
            # if we're not in the originating view for the vertex, hide the line for the
            # dimension we already know.
            if (my $new_vertex = $app->new_vertex) {
                if ($new_vertex->{from_view} && $current_view->name ne $new_vertex->{from_view}) {
                    # we can just hide both defined dimensions, one of them is already hidden.
                    for my $dim (0 .. 2) {
                        unless (defined($new_vertex->{vertex}->[$dim])) {

                            # we're changing a copy here
                            push @{ $skip->{$dim} }, $current_view->name;
                        }
                    }
                }
            }

            for my $line_index (keys %{ $skip }) {
                
                my $cursor_line = $app->system_thing->{"cursor_line_$line_index"};
                $cursor_line->{skip_view} = {};
                map { $cursor_line->{skip_view}->{$_} = 1; } @{ $skip->{$line_index} }; 
            }
        }

    }
};

__PACKAGE__->meta->make_immutable;

sub BUILD {
    my ($self, $args) = @_;

    # called to create the 'app' attribute of the wxApp object.
    # This class is the top of the Moose hierarchy and here we create the other Moose
    # objects we want.

    # create the default views
    my @views = (
        {
            name => 'XY',
            dims => [ 0, 1, 2 ],
            x_axis_direction => 1,
            y_axis_direction => -1,
            eye_direction => 1,
            debug => 1,
        },
        {
            name => 'XZ',
            dims => [ 0, 2, 1 ],
            x_axis_direction => 1,
            y_axis_direction => 1,
            eye_direction => 1,
        },
        {
            name => 'YZ',
            dims => [ 2, 1, 0 ],
            x_axis_direction => 1,
            y_axis_direction => -1,
            eye_direction => -1,
        },
        {
            name => 'perspective',
        },
    );

    my $cursor_image = Wx::Image->new('images/cursor32_circle2.png', wxBITMAP_TYPE_PNG, 0);
    $cursor_image->SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_X, 15);
    $cursor_image->SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_Y, 15);
    my $cursor = Wx::Cursor->new($cursor_image);

    for my $view ( @views ) {

        my $view_name = $view->{name};
        my %args;

        $args{name} = $view_name;

        if ($view->{dims}) {

            # orthogonal view
            ($args{x_dim}, $args{y_dim}, $args{other_dim}) = @{ $view->{dims} };

            # remove the combined dims setting and we can send all other keys thru
            delete $view->{dims};
            for my $key ( keys %{ $view } ) {
                $args{$key} = $view->{$key};
            }

        }

        # views are wrappers around the canvases
        push @{ $self->views }, $self->view->{$view_name} = WxBuildView->new(
            panel => $self->frame->{$view_name},
            %args,
        );

        $self->frame->{$view_name}->{canvas}->SetCursor($cursor);
    }

    my $axis_len = 50;
    my $cursor_size = $self->cursor_size;

    # create the items present in all views
    $self->system_thing({
        cursor_line_0 => { name => 'x axis cursor', type => $WxBuild::OT_LINE, color => [ 1, 0, 0 ], verts => [ [ $cursor_size, 0, 0 ], [ -$cursor_size, 0, 0 ], ], },
        cursor_line_1 => { name => 'y axis cursor', type => $WxBuild::OT_LINE, color => [ 0, 1, 0 ], verts => [ [ 0, $cursor_size, 0 ], [ 0, -$cursor_size, 0 ], ], },
        cursor_line_2 => { name => 'z axis cursor', type => $WxBuild::OT_LINE, color => [ 0, 0, 1 ], verts => [ [ 0, 0, $cursor_size ], [ 0, 0, -$cursor_size ], ], },
        vertex_line_0 => { debug => 1, name => 'yz vertex', type => $WxBuild::OT_LINE, color => [ 1, 1, 1 ], verts => [ [ 0, 0, 0 ], [ 0, 0, 0 ], ], },
        vertex_line_1 => { debug => 1, name => 'xz vertex', type => $WxBuild::OT_LINE, color => [ 1, 1, 1 ], verts => [ [ 100, 10, 5 ], [ 100, -10, 5 ], ], },
        vertex_line_2 => { debug => 1, name => 'xy vertex', type => $WxBuild::OT_LINE, color => [ 1, 1, 1 ], verts => [ [ 0, 0, 0 ], [ 0, 0, 0 ], ], },
#        xy_line => { name => 'x y line', type => $WxBuild::OT_LINE, color => [ 1, 0, 0 ], verts => [ [ -$cursor_size, 0, 0 ], [ 0, $cursor_size, 0 ], ], },
        x_axis => { name => 'x axis', type => $WxBuild::OT_LINE, color => [ 1, 0, 0 ], verts => [ [ -$axis_len, 0, 0 ], [ $axis_len, 0, 0 ], ], },
        y_axis => { name => 'y axis', type => $WxBuild::OT_LINE, color => [ 0, 1, 0 ], verts => [ [ 0, -$axis_len, 0 ], [ 0, $axis_len, 0 ], ],  debug => 1, },
        z_axis => { name => 'z axis', type => $WxBuild::OT_LINE, color => [ 0, 0, 1 ], verts => [ [ 0, 0, -$axis_len, ], [ 0, 0, $axis_len, ], ], },
        test1 => { name => 'test', type => $WxBuild::OT_LINE, color => [ 0, 1, 1 ], verts => [ [ -5, 0, 0, ], [ 0, 0, 0, ], ], }, 
        test2 => { name => 'test', type => $WxBuild::OT_LINE, color => [ 1, 0, 1 ], verts => [ [ -5, 1, 0, ], [ 0, 1, 0, ], ], }, 
        test3 => { name => 'test', type => $WxBuild::OT_LINE, color => [ 1, 1, 0 ], verts => [ [ -5, 2, 0, ], [ 0, 2, 0, ], ], }, 
        test4 => { name => 'test', type => $WxBuild::OT_LINE, color => [ 1, 0, 0 ], verts => [ [ -5, 3, 0, ], [ 0, 3, 0, ], ], }, 
        test5 => { name => 'test', type => $WxBuild::OT_LINE, color => [ 0, 1, 0 ], verts => [ [ -5, 4, 0, ], [ 0, 4, 0, ], ], }, 
        test6 => { name => 'test', type => $WxBuild::OT_LINE, color => [ 0, 0, 1 ], verts => [ [ -5, 5, 0, ], [ 0, 5, 0, ], ], },
    });

    # create a default model
    # $self->model(WxBuildModel->new(file => $args->{model_file}));
    $self->model(WxBuildModel->new());


#    push @{ $self->views }, $self->view->{view3d} = WxBuildView->new(
#        model => $self->model,
#        panel => $self->frame->{view3d});

}

################################################################################

sub refresh_all_views {
    my ($self) = @_;

    for my $view (qw(XY XZ YZ perspective)) {
#        $self->view->{$view}->canvas->dirty(1);
        $self->view->{$view}->refresh;
    }

    return;
}

################################################################################

sub change_cursor_state {
    my ($self, $event, $state) = @_;
    $log->debug("change_cursor_state: @_");
    my $current_state = $self->cursor_state;
    if ($event->IsChecked) {
        $current_state |= $state;
    }
    else {
        $current_state &= ~$state;
    }

    $log->debug("state now $current_state");
    $self->cursor_state($current_state);

}

################################################################################

sub move_cursor {
    my ($self, $coords) = @_;

    for my $dim (0 .. 2) {
        if (defined $coords->[$dim]) {
            $self->cursor->dim($dim, $coords->[$dim]);

            # update the coord in the status bar
            $self->frame->set_status_text($dim + 2, $coords->[$dim]);

            # move any items following the cursor
            if ($self->cursor_state) {
                $self->view->{perspective}->eye->dim($dim, $coords->[$dim]) if ($self->cursor_state & $CS_EYE);
                $self->view->{perspective}->light->dim($dim, $coords->[$dim]) if ($self->cursor_state & $CS_LIGHT);
                $self->view->{perspective}->target->dim($dim, $coords->[$dim]) if ($self->cursor_state & $CS_TARGET);
            }
        }
    }

#    $self->adjust_cursor_lines($coords);

    $self->refresh_all_views;

    return;
}

################################################################################

sub adjust_cursor_lines {
    my ($self, $coords) = @_;

    my $cursor_size = $self->cursor_size;

    for my $dim (0 .. 2) {

#            # span the views in this dimension
#            $vertex_line->{verts}->[0]->[$i] = -100;
#            $vertex_line->{verts}->[1]->[$i] = 100;
#
#            # use the defined variable or a substitute (0 or infinity)
#            for my $inc (1,2) {
#                my $other_dim = ($i + $inc) % 3;
#                if (defined $model_coords[$other_dim]) {
#                    $vertex_line->{verts}->[0]->[$other_dim] = $model_coords[$other_dim];
#                    $vertex_line->{verts}->[1]->[$other_dim] = $model_coords[$other_dim];
#                }
#                else {
#                    $vertex_line->{verts}->[0]->[$other_dim] = $self->eye_direction * 1000;
#                    $vertex_line->{verts}->[1]->[$other_dim] = $self->eye_direction * 1000;
#                }
#            }

        my $cursor_line = $self->system_thing->{"cursor_line_$dim"};
        if (defined $coords->[$dim]) {
            $cursor_line->{verts}->[0]->[$dim] = $coords->[$dim] - $cursor_size;
            $cursor_line->{verts}->[1]->[$dim] = $coords->[$dim] + $cursor_size;
        }

        for my $inc (1,2) {
            my $other_dim = ($dim + $inc) % 3;
            if (defined $coords->[$other_dim]) {
                $cursor_line->{verts}->[0]->[$other_dim] = $coords->[$other_dim];
                $cursor_line->{verts}->[1]->[$other_dim] = $coords->[$other_dim];
            }
            else {
                $cursor_line->{verts}->[0]->[$other_dim] = $self->views->[$dim]->eye_direction * 1000;
                $cursor_line->{verts}->[1]->[$other_dim] = $self->views->[$dim]->eye_direction * 1000;
            }
        }

    }

    return;
}

################################################################################

sub
refresh_object_list
{
    my ($self, @selected_things) = @_;

    # the ability to refresh and retain the current selection doesn't seem to be
    # used at present.
    my %selected_thing;
    map { $selected_thing{$_} = 1; } @selected_things;

    my $tree = $self->frame->{tree};
    $tree->DeleteAllItems;
    my $root = $tree->AddRoot('root');
    for my $thing (@{ $self->model->things }) {
        if ($thing->{listed}) {
            $log->debug("add tree node for $thing->{name}");
            my $new_node = $tree->AppendItem($root, $thing->{name});
            if ($selected_thing{$thing}) {
                $tree->SelectItem($new_node);
            }
        }
    }

}

################################################################################

sub
update_listed_thing_indices
{
    my ($self) = @_;

    my @indices = ();
    my $index = 0;
    for my $thing (@{ $self->model->things }) {
        if ($thing->{listed}) {
            push @indices, $index;
        }
        $index++;
    }
    $log->debug("listed_thing_indices: @indices");
    $self->listed_thing_indices(\@indices);
    return;
}

################################################################################
# 
# sub
# update_selected_flags
# {
# 
#     # get the list of selected listbox lines and turn that into a list of @thing indices by
#     # going through the objects and 
#     map { $things[$_]->{_selected} = 0; } (0 .. $#things);
#     my @selected_thing_indices = ();
#     if (my @selected_lines = $object_list->curselection()) {
#         @selected_thing_indices = @listed_thing_indices[@selected_lines];
#         for my $i (@selected_thing_indices) {
#             $things[$i]->{_selected} = 1;
#             if ($things[$i]->{_series_things}) {
#                 # the series widgets follow the owner widget in the list
#                 my $nbr_series_widgets = @{ $things[$i]->{_series_things} };
#                 map { $things[$_]->{selected} = 1; } (($i + 1) .. ($i + $nbr_series_widgets));
#                 push @selected_thing_indices, (($i + 1) .. ($i + $nbr_series_widgets));
#             }
#         }
#     }
# 
#     # return this so we can operate on just the selected things if we want to
#     return reverse @selected_thing_indices;
# }
# 
# ################################################################################
# 
# sub
# clear_all(@) 
# {
#     $object_list->delete(0, 'end');
#     @things = ();
#     @listed_thing_indices = ();
#     $new_thing = undef;
#     queue_gl_draw();
# }
 
################################################################################
# 
# sub
# show_series_properties
# {
#     my ($thing, $series_type) = @_;
#     
#     $log->debug("show_series_properties $thing $series_type");
#     if ($thing->{_series_widgets}) {
#         if ( my $first_widget = shift @{ $thing->{_series_widgets} }) {
#             $first_widget->gridForget( @{ $thing->{_series_widgets} } );
#         }
#     }
# 
#     # specify controls required to define series of each type
#     my $series_type_controls = {
#         none => [ ],
#         line => [
#             [
#                 [ 'Entries', 'NumEntry', { -minvalue => 1, increment => 1, }, 10 ], 
#             ],
#             [
#                 [ 'XInc', 'NumEntry' ], 
#                 [ 'YInc', 'NumEntry' ], 
#                 [ 'ZInc', 'NumEntry' ], 
#             ],
#         ],
#         arc => [
#             [
#                 [ 'Entries', 'NumEntry', { -minvalue => 1, increment => 1, }, 10 ], 
#             ],
#             [
#                 [ 'AngleStart', 'NumEntry', { -minvalue => 0, -maxvalue => 360, -increment => 5, }, 0 ],
#                 [ 'AngleChange', 'NumEntry', { -minvalue => -180, -maxvalue => 180, -increment => 5, }, 30 ],
#             ],
#             [
#                 [ 'SideLength', 'NumEntry', { -minvalue => 0.5, -increment => 0.5, }, 4 ], 
#                 [ 'Centred', 'Checkbutton', {} ], 
#             ],
#             [
#                 [ 'Plane', 'Radiobutton', { -text => 'XY', -value => 'XY', }, 'XZ' ], 
#                 [ 'Plane', 'Radiobutton', { -text => 'XZ', -value => 'XZ', } ], 
#                 [ 'Plane', 'Radiobutton', { -text => 'YZ', -value => 'YZ', } ], 
#             ],
#         ],
#         surface => [ ],
#         cloud => [
#             [
#                 [ 'XEntries', 'NumEntry', { -minvalue => 1, increment => 1, } ], 
#                 [ 'YEntries', 'NumEntry', { -minvalue => 1, increment => 1, } ], 
#                 [ 'ZEntries', 'NumEntry', { -minvalue => 1, increment => 1, } ], 
#             ],
#             [
#                 [ 'XInc', 'NumEntry', {}, 1 ], 
#                 [ 'YInc', 'NumEntry', {}, 1 ], 
#                 [ 'ZInc', 'NumEntry', {}, 1 ], 
#             ],
#             [
#                 [ 'Conditions', 'Button', { -command => \&edit_text, }, "", ],
#             ],
#             [
#                 [ 'XCentred', 'Checkbutton', {}, 1 ], 
#                 [ 'YCentred', 'Checkbutton', {}, 0 ], 
#                 [ 'ZCentred', 'Checkbutton', {}, 1 ], 
#             ],
#             [
#                 [ 'HideOriginal', 'Checkbutton', {}, 1 ], 
#             ],
#         ],
#     };
# 
#     # widget properties by type for this screen
#     my $series_widget_type_properties = {
#         NumEntry => {
#             -browsecmd => [ \&adjust_series, $thing ],
#             -command => [ \&adjust_series, $thing ],
#             -increment => 1,
#         },
#         Checkbutton => {
#             -command => [ \&adjust_series, $thing ],
#         },
#         Radiobutton => {
#             -command => [ \&adjust_series, $thing ],
#         },
#         Button => {
#             -text => '...',
#         },
#     };
# 
#     display_type_widgets($thing, 'series', $series_type_controls, $series_type, $series_widget_type_properties);
# 
#     # display according to current settings
#     adjust_series($thing);
# 
#     # fixup when reselecting
#     $thing->{_properties_win}->update;
#     my $width = $thing->{_properties_nb}->width;
#     my $height = $thing->{_properties_nb}->height;
#     $log->debug("$width $height");
#     $thing->{_properties_win}->geometry("${width}x${height}") if $width > 1;
# 
#     return;
# }
# 
# ################################################################################
# 
# sub
# show_transform_properties
# {
#     my ($thing, $transform_type) = @_;
# 
#     $log->debug("show_transform_properties $thing $transform_type");
#     if ($thing->{_transform_widgets}) {
#         if ( my $first_widget = shift @{ $thing->{_transform_widgets} }) {
#             $first_widget->gridForget( @{ $thing->{_transform_widgets} } );
#         }
#     }
# 
#     # specify controls required to define transform of each type
#     my $transform_type_controls = {
#         none => [ ],
#         prism => [
#             [
#                 [ 'Sides', 'NumEntry', { -minvalue => 3, increment => 1, }, 5 ], 
#             ],
#             [
#                 [ 'Plane', 'Radiobutton', { -text => 'XY', -value => 'XY', }, 'XZ' ], 
#                 [ 'Plane', 'Radiobutton', { -text => 'XZ', -value => 'XZ', } ], 
#                 [ 'Plane', 'Radiobutton', { -text => 'YZ', -value => 'YZ', } ], 
#             ],
#             [
#                 [ 'Angle', 'NumEntry', { -minvalue => 0, -maxvalue => 360, }, 0 ], 
#             ],
#             [
#                 [ 'Size', 'Radiobutton', { -text => 'Small', -value => 'Small', }, 'Small' ], 
#                 [ 'Size', 'Radiobutton', { -text => 'Large', -value => 'Large', } ], 
#                 [ 'Size', 'Radiobutton', { -text => 'Custom', -value => 'Custom', } ], 
#             ],
#             [
#                 [ 'CustomSize', 'NumEntry', { -minvalue => 0.1, increment => 0.5, }, 2 ], 
#             ],
#             [
#                 [ 'NegativeEndSize', 'NumEntry', { -minvalue => 0, -increment => 0.1, }, 1 ], 
#                 [ 'PositiveEndSize', 'NumEntry', { -minvalue => 0, -increment => 0.1, }, 1 ], 
#             ],
#         ],
#         stairs => [
#             [
#                 [ 'Steps', 'NumEntry', { -minvalue => 1, increment => 1, }, 1 ], 
#             ],
#             [
#                 [ 'Plane', 'Radiobutton', { -text => 'XY', -value => 'XY', }, 'XY' ], 
#                 [ 'Plane', 'Radiobutton', { -text => 'XZ', -value => 'XZ', } ], 
#                 [ 'Plane', 'Radiobutton', { -text => 'YZ', -value => 'YZ', } ], 
#             ],
#             [
#                 [ 'Rotation', 'NumEntry', { -minvalue => 1, -maxvalue => 4, increment => 1, }, 1 ], 
#             ],
#         ],
#         point => [ ],
#         arch => [ ],
#     };
# 
#     # widget properties by type for this screen
#     my $transform_widget_type_properties = {
#         NumEntry => {
#             -browsecmd => [ \&adjust_transform, $thing ],
#             -command => [ \&adjust_transform, $thing ],
#             -increment => 1,
#         },
#         Checkbutton => {
#             -command => [ \&adjust_transform, $thing ],
#         },
#         Radiobutton => {
#             -command => [ \&adjust_transform, $thing ],
#         },
#     };
# 
#     display_type_widgets($thing, 'transform', $transform_type_controls, $transform_type, $transform_widget_type_properties);
# 
#     # display according to current settings
#     adjust_transform($thing);
# 
#     # fixup when reselecting
#     $thing->{_properties_win}->update;
#     my $width = $thing->{_properties_nb}->width;
#     my $height = $thing->{_properties_nb}->height;
#     $log->debug("$width $height");
#     $thing->{_properties_win}->geometry("${width}x${height}") if $width > 1;
# 
#     return;
# }
# 
# ################################################################################
# 
# sub
# show_properties
# {
#     my $thing = shift;
# 
#     my $properties_win;
#     if (Exists($thing->{_properties_win})) {
#         $properties_win = $thing->{_properties_win};
#         $properties_win->deiconify();
#         $properties_win->raise();
#         $thing->{_properties_tab}->gridForget(@{ $thing->{_property_widgets} });
#     }
#     else {
#         $thing->{_properties_win} = $properties_win = $main_win->Toplevel();
#         $properties_win->transient($main_win);
#         $properties_win->protocol('WM_DELETE_WINDOW', sub { $properties_win->withdraw });
#         $properties_win->bind("<KeyPress>", [ \&key_handler, Ev('K') ] );
# 
#         my $properties_nb = $thing->{_properties_nb} = $properties_win->NoteBook()->grid('-','-','-', -sticky => 'nsew');
#         $thing->{_properties_tab} = $properties_nb->add('properties', -label => 'Properties');
# 
#         # transform properties
#         $thing->{_transform_tab} = $properties_nb->add('transform', -label => 'Transform');
#         $thing->{transform_type} ||= 'none';
#         $thing->{_transform_tab}->Label(-text => 'Type', -font => $small_bold_font)->grid(
#             $thing->{_transform_tab}->Optionmenu(  
#                 -variable => \$thing->{transform_type},
#                 -options => [ qw(none prism stairs point arch) ],
#                 -command => [ \&show_transform_properties, $thing ],
#             ),
#         );
#         $thing->{_transform_frame} = $thing->{_transform_tab}->Frame()->grid('-');
# 
#         # series properties
#         $thing->{_series_tab} = $properties_nb->add('series', -label => 'Series');
#         $thing->{series_type} ||= 'none';
#         $thing->{_series_tab}->Label(-text => 'Type', -font => $small_bold_font)->grid(
#             $thing->{_series_tab}->Optionmenu(  
#                 -variable => \$thing->{series_type},
#                 -options => [ qw(none line arc surface cloud) ],
#                 -command => [ \&show_series_properties, $thing ],
#             ),
#         );
#         $thing->{_series_frame} = $thing->{_series_tab}->Frame()->grid('-');
#     }
# 
# #    $log->debug("properties for $thing->{name}: " . Dumper($thing));
# 
#     $properties_win->title("Properties for $thing->{name}");
#     $thing->{_property_widgets} = [];
# 
#     foreach my $property (sort keys %$thing) {
#         next if $property =~ /^_/;
#         my @widgets = ();
#         my @spans = ();
# 
#         if (ref(\$thing->{$property}) eq 'SCALAR') {
#             # default for simple variables is a simple entry field 
#             push @widgets, $thing->{_properties_tab}->Entry(
#                 -font => $small_font,
#                 -textvariable => \$thing->{$property},
#                 -background => 'white',
#                 -foreground => 'black',
#                 );
#             @spans = (qw/- - - - -/);
#         }
#         elsif (ref $thing->{$property} eq 'ARRAY') {
#             $log->debug("display array for $property");
#             push @widgets, my $frame = $thing->{_properties_tab}->Frame();
#             my @subwidgets = ();
#             for my $index (0 .. $#{ $thing->{$property} }) {
#                 push @subwidgets, $frame->Entry(
#                     -font => $small_font,
#                     -textvariable => \$thing->{$property}->[$index],
#                     -width => 5,
#                     -background => 'white',
#                 );
#             }
#             my $first_widget = shift @subwidgets;
#             $first_widget->grid(@subwidgets) if $first_widget;
#         }
#         elsif (ref $thing->{$property} eq 'HASH') {
#             $log->debug("display hash for $property");
#             push @widgets, my $frame = $thing->{_properties_tab}->Frame();
#             my @subwidgets = ();
#             for my $key (sort keys %{ $thing->{$property} }) {
#                 push @subwidgets, $frame->Label(
#                     -font => $small_bold_font,
#                     -text => $key,
#                     );
#                 push @subwidgets, $frame->Entry(
#                     -width => 5,
#                     -background => 'white',
#                     -font => $small_font,
#                     -textvariable => \$thing->{$property}->{$key},
#                 );
#             }
#             my $first_widget = shift @subwidgets;
#             $first_widget->grid(@subwidgets) if $first_widget;
#         }
#         else {
#             # everything else gets a label
#             push @widgets, $thing->{_properties_tab}->Label(-text => sprintf("%s",$thing->{$property}));
#             @spans = (qw/- - - - -/);
#         }
# 
#         push @{ $thing->{_property_widgets} },
#             $thing->{_properties_tab}->Button(
#                     -text => "$property",  
#                     -relief => 'groove', 
#                     -command => sub { queue_gl_draw(); },
#                     -width => 15,
#                     -pady => 0,
#                     -font => $small_bold_font,
#             )->grid(@widgets, @spans, -sticky => 'w');
#         push @{ $thing->{_property_widgets} }, @widgets;
# 
# 
#     }
# 
#     $thing->{_properties_win}->update;
#     show_series_properties($thing, $thing->{series_type});
#     show_transform_properties($thing, $thing->{transform_type});
# #    push @property_widgets, $properties_win->Button(
# #        -text => "Render", -command => sub { render_scene(refresh_source => 1); }
# #            )->grid(-columnspan => 7, -sticky => 'ew');
# 
# }
# 
# ################################################################################
# 
# sub
# object_list_select_handler
# {
#     my ($object_lb) = @_;
# 
#     refresh_views();
#     queue_gl_draw() if $gl_setting->{render_by_selection};
# }
# 
# ################################################################################
# 
# sub
# get_key_factor 
# {
#     my $factor;
#     if ($current_text =~ /^[.\d]+$/) {
#         $factor = $current_text;
#     }
#     else {
#         $factor = 1;
#     }
#     $factor *= 10 if $shift_key_down;
#     $factor /= 10 if $control_key_down;
#     return $factor;
# }
# 
# ################################################################################
# 
# sub
# key_handler
# {
#     my ($entry, $keyname) = @_;
# 
#     # easy way of describing keys that just toggle flags
#     my $flag_toggle_keys = {
#         e => \$gl_setting->{eye_follows_cursor},
#         l => \$gl_setting->{light_follows_cursor},
#         t => \$gl_setting->{target_follows_cursor},
#         S => \$filter_shown_by_position,
#     };
# 
#     my $key_actions = {
#         q => sub { $main_win->destroy; },
#         n => sub {
#             $current_text = '';
#             $text_mode = $TM_NAME;
#         },
#         d => sub { $divisions = $divisions < 10 ? $divisions + 1 : 2; refresh_views(); },
#         o => sub { 
#             # o for origin
#             for my $view (values %flat_view) {
#                 my @model_origin = (0,0,0);
#                 ($view->{snap_x}, $view->{snap_y}) = model_xyz_to_view_xy($view, @model_origin);
#             }
#             move_tracker($current_view->{canvas}, $current_view->{snap_x}, $current_view->{snap_y}, { no_snap => 1 } );
#         },
#         Escape => sub { $current_text = ""; $text_mode = $TM_NORMAL; },
#         Return => sub {
# 
#             if ($text_mode == $TM_NAME) {
#                 # use the current text as the name for all selected objects
#                 if ($current_text) {
#                     for my $index ($object_list->curselection()) {
#                         $object_list->delete($index);
#                         $object_list->insert($index, $current_text);
#                         $object_list->selectionSet($index);
#                     }
#                     map { $things[$_]->{name} = $current_text; } update_selected_flags();
#                 }
#                 $text_mode = $TM_NORMAL;
#                 $current_text = "";
#             }
#             else {
# 
#                 # fake a click at the current snap position
#                 click_tracker($current_view->{canvas}, 0, $current_view->{snap_x}, $current_view->{snap_y} );
#                 click_tracker($current_view->{canvas}, 1, $current_view->{snap_x}, $current_view->{snap_y} );
#             }
#         },
#         Left => sub { my $f = get_key_factor(); change_view_snap ($current_view, -1*$f,0); },
#         Right => sub { my $f = get_key_factor(); change_view_snap ($current_view, 1*$f,0); },
#         Up => sub { my $f = get_key_factor(); change_view_snap ($current_view, 0,1*$f); },
#         Down => sub { my $f = get_key_factor(); change_view_snap ($current_view, 0,-1*$f); },
#         Delete => sub {
#             for my $i (update_selected_flags()) {
#                 my $thing = $things[$i];
#                 splice @things, $i, 1;
#             }
#             for my $i (reverse $object_list->curselection()) {
#                 $object_list->delete($i);
#             }
#             update_listed_thing_indices();
#             queue_gl_draw();
#         },
#         BackSpace => sub {
#             $log->debug("backspace: $current_text");
#             if (length($current_text)) {
#                 $current_text =~ s/.$//;
#                 $log->debug("backspaced: $current_text");
#             }
#         },
# #        space => sub { $current_text .= ' ' if $text_mode == $TM_NAME; },
# #        underscore => sub { $current_text .= '_' if $text_mode == $TM_NAME; },
#         p => sub {
#             for my $i (update_selected_flags()) {
#                 next if $things[$i]->{do_not_save};
#                 show_properties($things[$i]);
#             }
#         },
# 
#         # next drag action defines shown area
#         s => sub { $drag_to_show_flag = ! $drag_to_show_flag; },
#             
#     };
# 
#     # add toggle actions
#     for my $key (keys %{ $flag_toggle_keys } ) {
#         $key_actions->{$key} = sub { ${ $flag_toggle_keys->{$key} } = ! ${ $flag_toggle_keys->{$key} } };
#     }
# 
#     # change named printables into the real chars
#     my %named_printables = (
#         space => ' ',
#         underscore => '_',
#         period => '.',
#     );
#     $keyname = $named_printables{$keyname} if $named_printables{$keyname};
# 
#     my $mode_key_regex = {
# 
#         # everything fires in normal mode
#         $TM_NORMAL => qr/./o,
# 
#         # special keys for name editing
#         $TM_NAME => qr/Return|BackSpace|Escape|space|underscore/o,
#     };
# 
#     # perform key actions if the mode supports that key
#     if ((my $sub = $key_actions->{$keyname}) && $keyname =~ $mode_key_regex->{$text_mode}) {
#         $log->debug("doing sub for $keyname");
#         $sub->();
#     }
#     elsif ($keyname =~ /\A[.\d]\z/ || ($text_mode == $TM_NAME && $keyname =~ /^[\w\s_]$/)) {
# 
#         # in normal mode accumulate numbers only as input for the next action,
#         # in name mode, use standard identifier chars
#         $current_text .= $keyname;
#         $log->debug("add char $keyname");
#     }
#     else {
#         $log->debug("key $keyname");
#     }
# 
#     # update status message according to mode
#     if ($text_mode == $TM_NAME) {
#         $msg_statusvar = 'Enter object name';
#     }
#     else {
#         $msg_statusvar = '';
#     }
# 
#     $status_entry->icursor('end');
# 
#     # hmmm
#     if ($keyname =~ /\AS\z/) {
#         refresh_views();
#     }
# 
# }
# 
# ################################################################################
# 
# sub
# init_tk() 
# {
# 
#     # generic list for checkbox control over a group of controls
#     my @controls;
# 
#     $main_win = MainWindow->new(-background => 'black');
#     $main_win->geometry("800x650+210+50");
#     my @label_opts = ( -relief => 'sunken', -width => 5, -font => $small_font);
# 
#     my $status_frame = $main_win->Frame(-background => 'darkgreen')->pack(-fill => 'x', -side => 'bottom');
#     $status_frame->Label(-text => "Cursor", -font => $small_bold_font)->pack(
#         $status_frame->Label(-textvariable => \$cursor_position[0], @label_opts),
#         $status_frame->Label(-textvariable => \$cursor_position[1], @label_opts),
#         $status_frame->Label(-textvariable => \$cursor_position[2], @label_opts),
#         $status_frame->Label(-text => "Selected", -font => $small_bold_font),
#         $status_frame->Label(-textvariable => \$selected_statusvar[0], @label_opts),
#         $status_frame->Label(-textvariable => \$selected_statusvar[1], @label_opts),
#         $status_frame->Label(-textvariable => \$selected_statusvar[2], @label_opts),
#         $status_frame->Label(-text => "Divs", -font => $small_bold_font),
#         $status_frame->Label(-textvariable => \$divisions, @label_opts),
#         $status_entry = $status_frame->Entry(-textvariable => \$current_text, -relief => 'sunken', -width => 20, -background => 'white'),
#         $status_frame->Label(-textvariable => \$msg_statusvar, -relief => 'sunken', -width => 80, -anchor => 'w'),
#         -side => 'left', -expand => 1, -fill => 'both',
#         );
#         
#     # we want custom keystroke handling for the status entry widget so we can 
#     # react to arrows, enter, and so we control what is displayed instead of just dumping the keys pressed
#     $status_entry->bindtags(undef);
#     $status_entry->bind("<KeyPress>", [ \&key_handler, Ev('K') ] );
# 
#     $toolbar = $main_win->Frame->pack(-anchor => 'w', -fill => 'x', -expand => 0);
# 
#     $toolbar->Button(-takefocus => 0, -font => $small_bold_font,
#             -text => "S", -command => sub { 
#                 if (Exists($source_win)) {
#                     $source_win->deiconify();
#                     $source_win->raise();
#                 }
#                 else {
#                     # create the source window
#                     $source_win = $main_win->Toplevel();
#                     $source_win->transient($main_win);
#                     $source_win->geometry("400x600+800+300");
#                     $source_win->title("POV Source");
#                     $source_win->Button( -text => "Hide", -command => sub { $source_win->withdraw })->grid(
#                         $source_win->Button( -text => "Render", -command => \&render_scene),
#                         $source_win->Button( -text => "Open", -command => \&open_pov_file),
#                         $source_win->Button( -text => "Save", -command => \&save_pov_file),
#                         -sticky => 'ew');
# 
#                     my $source_nb = $source_win->NoteBook()->grid('-','-','-', -sticky => 'nsew');
#                     my $source_tab = $source_nb->add('source', -label => 'Source');
#                     $source_text = $source_tab->Scrolled("Text", -scrollbars => 'osoe',
#                         -width => 50, -wrap => 'none')->pack(-expand => 1, -fill => 'both');
#                     my $output_tab = $source_nb->add('output', -label => 'Output');
#                     $output_text = $output_tab->Scrolled("ROText", -scrollbars => 'osoe',
#                         -width => 50, -wrap => 'none')->pack(-expand => 1, -fill => 'both');
# 
#                     $source_win->gridColumnconfigure(0, -weight => 1);
#                     $source_win->gridColumnconfigure(1, -weight => 1);
#                     $source_win->gridColumnconfigure(2, -weight => 1);
#                     $source_win->gridColumnconfigure(3, -weight => 1);
#                     $source_win->gridRowconfigure(1, -weight => 1);
# 
#                     $current_source = get_povray_source($path_to_thing{''});
#                     $source_text->insert('end',$current_source);
#                 }
#              }
#         )->pack(
# 
#         $toolbar->Button(-takefocus => 0, -font => $small_bold_font,
#                 -text => "R", -command => sub
#             {
#                 render_scene(refresh_source => 1);
#             }),
# 
#         $toolbar->Button(
#             -takefocus => 0, 
#             -font => $small_bold_font,
#             -text => "Color", 
#             -command => sub { 
# 
#                 if ( my $color = $main_win->chooseColor( ) ) {
#                     $log->debug("color = $color");
#                     my @rgb = ($color =~ /([a-f0-9]{4})/g);
#                     map { $_ = hex($_) } @rgb;
#                     map { $_ /= 0xFFFF } @rgb;
#                     $log->debug("rgb = " . Dumper(\@rgb));
# 
#                 }
# 
#             },
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0, 
#             -font => $small_bold_font,
#             -text => "Test", 
#             -command => sub { queue_gl_draw(); }
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0, 
#             -font => $small_bold_font,
#             -text => "Open", 
#             -command => \&open_from_file,
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0, 
#             -font => $small_bold_font,
#             -text => "Save", 
#             -command => \&save_to_file,
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0, 
#             -font => $small_bold_font,
#             -text => "Save As...", 
#             -command => sub { $filename = undef; save_to_file(); },
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0, 
#             -font => $small_bold_font,
#             -text => "Clear", 
#             -command => sub { clear_all(); $filename = undef; },
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0, 
#             -font => $small_bold_font, 
#             -text => 'Toolbox',
#             -command => sub { $toolbox_win->deiconify(); $toolbox_win->raise(); },
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0, 
#             -font => $small_bold_font,
#             -text => 'Copy', 
#             -command => [ \&copy_current ],
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0,
#             -font => $small_bold_font,
#             -text => "Delete",
#             -command => [ \&delete_current ],
#         ),
# 
#         $toolbar->Button(
#             -takefocus => 0,
#             -font => $small_bold_font,
#             -text => "Debug",
#             -command => sub { $debug = ! $debug; },
#         ),
# 
#         -side => 'left');
# 
#     my $pw = $main_win->Panedwindow->pack(-side => 'top', -expand => 'yes', -fill => 'both');
#     $control_frame = $pw->Frame(-relief => 'sunken', -borderwidth => 2);
#     $view_frame = $pw->Frame(-relief => 'sunken', -borderwidth => 2, -background => 'white');
#     $pw->add($control_frame);
#     $pw->add($view_frame);
# 
#     # create the list
#     $object_list = $control_frame->Scrolled('Listbox', -scrollbars => 'osoe',
#         -takefocus => 0,
#         -selectmode => 'extended',
#         );
# 
#     $object_list->pack(-side => 'top', -fill => 'both', -expand => 1);
#     $object_list->bind('<<ListboxSelect>>', \&object_list_select_handler);
#     $object_list->bind('<Double-Button-1>',
#         sub {
#             for my $i (update_selected_flags()) {
#                 next if $things[$i]->{do_not_save};
#                 show_properties($things[$i]);
#             }
#         });
# 
#     $main_win->update;
# 
#     $toolbox_win = $main_win->Toplevel();
#     $toolbox_win->transient($main_win);
#     $toolbox_win->title("Toolbox");
#     $toolbox_win->withdraw;
# #    $toolbox_win->Button( -text => "Close", -command => sub { $toolbox_win->withdraw })->grid(-columnspan => 7, -sticky => 'ew');
#     $toolbox_win->protocol('WM_DELETE_WINDOW', sub { $toolbox_win->withdraw });
#     $toolbox_win->bind("<KeyPress>", [ \&key_handler, Ev('K') ] );
#     my $toolbox_nb = $toolbox_win->NoteBook()->grid('-','-','-', -sticky => 'nsew');
# 
#     # 'options' settings 
# 
#     my $option_tab = $toolbox_nb->add('options', -label => 'Options');
#     $option_tab->Checkbutton( 
#         -text => 'Debug', 
#         -variable => \$debug,
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid();
#     $option_tab->Checkbutton( 
#         -text => 'Gravity', 
#         -variable => \$drop_unsupported,
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid();
# 
#     # 'move' settings 
# 
#     my $move_tab = $toolbox_nb->add('move', -label => 'Move');
# 
#     my $fr = $move_tab->Frame(-relief => 'raised', -borderwidth => 2)->grid('-');
#     $fr->Label(-text => 'Order', -font => $small_bold_font)->grid('-','-');
#     # create the order controls
#     $move_asc_order{x} = $move_asc_order{y} = $move_asc_order{z} = 1;
# 
#     $move_order_lb = $fr->Listbox(-takefocus => 0, -height => 3 ,-width => 3, -font => $small_bold_font)->grid(
#         -row => 1, -column => 0, -rowspan => 3);
#     $move_order_lb->insert('end', qw(X Y Z));
#     # note that this type of command binding automatically passes the lb ref as the first arg
#     $move_order_lb->bind('<ButtonRelease-1>' => [ \&move_list_item_down ] );
# 
#     for (0..5) {
#         my @text = ('X Asc', 'Desc', 'Y Asc', 'Desc', 'Z Asc', 'Desc');
#         my @vars = (\$move_asc_order{x}, \$move_asc_order{y}, \$move_asc_order{z});
#         my @vals = (1,0);
#         $fr->Radiobutton(
#             -takefocus => 0, 
#             -text => $text[$_],
#             -variable => $vars[int($_/2)],
#             -font => $small_bold_font,
#             -value => $vals[$_ % 2],
#             )->grid(
#                 -row => int($_/2)+1,
#                 -column => ($_ % 2) + 1,
#                 );
#     }
# 
#     foreach (qw(x y z)) {
# 
#         # create these controls first so the main cb can change them
#         @controls = ();
#         push @controls, $move_tab->Checkbutton(
#             -text => 'Dimension',
#             -variable => \$move_dim_flag{$_},
#             -state => 'disabled',
#             %{ $default_widget_type_properties->{Checkbutton} },
#             );
# 
#         push @controls, $move_tab->NumEntry(
#             -minvalue => -$GDIM{$_} + 1,
#             -maxvalue => $GDIM{$_} - 1,
#             -textvariable => \$move_start_amount{$_},
#             -state => 'disabled',
#             %{ $default_widget_type_properties->{NumEntry} },
#             );
# 
#         push @controls, $move_tab->NumEntry(
#             -minvalue => -$GDIM{$_} + 1,
#             -maxvalue => $GDIM{$_} - 1,
#             -textvariable => \$move_loop_amount{$_},
#             -state => 'disabled',
#             %{ $default_widget_type_properties->{NumEntry} },
#             );
# 
#         # now create the controlling checkbox
#         $move_tab->Checkbutton(
#             -text => "Adjust " . uc($_),
#             -variable => \$move_adjust_flag{$_},
#             -command => [ \&control_switcher, \$move_adjust_flag{$_}, @controls ],
#             %{ $default_widget_type_properties->{Checkbutton} },
#             )->grid($controls[0], -sticky => 'w');
# 
#         # lay out the fields
#         $move_tab->Label( -text => "Start", -font => $small_bold_font)->grid(
#             $move_tab->Label( -text => "Loop", -font => $small_bold_font),
#             );
#         $controls[1]->grid(
#             $controls[2],
#         #     -sticky => 'w'
#             );
#     }
# 
#     $move_tab->Button(
#         -text => 'Move',
#         -font => $small_bold_font,
#         -takefocus => 0,
#         -command => \&move_current)->grid('-');
# 
#     # enable column resizing
#     $move_tab->gridColumnconfigure(0, -weight => 1);
#     $move_tab->gridColumnconfigure(1, -weight => 1);
#     # $move_tab->gridColumnconfigure(2, -weight => 1);
# 
#     # 'select' settings
# 
#     my $select_tab = $toolbox_nb->add('select', -label => 'Select');
# 
#     # create the position filter fields first so the checkbutton can turn them on & off
#     @controls = ();
#     foreach (0..5) {
#         $select_pos_var[$_] = (0,$GX-1,0,$GY-1,0,$GZ-1)[$_];
#         # no command callbacks, we aren't changing the current selection
#         push @controls, $select_tab->NumEntry(
#             -minvalue => 0,
#             -maxvalue => $_ == 0 ? $GX-1 : $_ == 1 ? $GY-1 : $GZ-1,
#             -textvariable => \$select_pos_var[$_],
#             -state => 'disabled',
#             %{ $default_widget_type_properties->{NumEntry} },
#             );
#     }
#     # create the checkbutton which enables/disables the fields
#     $select_tab->Checkbutton(
#         -text => 'Position',
#         -variable => \$select_pos_flag,
#         -command => [ \&control_switcher, \$select_pos_flag, @controls ],
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid(
#             $select_tab->Label(-text => 'Min', -font => $small_bold_font),
#             $select_tab->Label(-text => 'Max', -font => $small_bold_font),
#             );
#     # now lay out the fields
#     $select_tab->Label(-text => 'X', -font => $small_bold_font)->grid($controls[0], $controls[1], -sticky => 'e');
#     $select_tab->Label(-text => 'Y', -font => $small_bold_font)->grid($controls[2], $controls[3], -sticky => 'e');
#     $select_tab->Label(-text => 'Z', -font => $small_bold_font)->grid($controls[4], $controls[5], -sticky => 'e');
# 
#     # create the size filter fields first so the checkbutton can turn them on & off
#     @controls = ();
#     foreach (0..5) {
#         $select_size_var[$_] = (1,$GX,1,$GY,1,$GZ)[$_];
#         # no command callbacks, we aren't changing the current selection
#         push @controls, $select_tab->NumEntry(
#             -minvalue => 1,
#             -maxvalue => $_ == 0 ? $GX : $_ == 1 ? $GY : $GZ,
#             -textvariable => \$select_size_var[$_],
#             -state => 'disabled',
#             %{ $default_widget_type_properties->{NumEntry} },
#             );
#     }
#     # create the checkbutton which enables/disables the fields
#     my $cb = $select_tab->Checkbutton(
#         -text => 'Size',
#         -variable => \$select_size_flag,
#         -command => [ \&control_switcher, \$select_size_flag, @controls ],
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid(
#         $select_tab->Label(-text => 'Min', -font => $small_bold_font),
#         $select_tab->Label(-text => 'Max', -font => $small_bold_font),
#         );
#     # re-grid the cb to position it correctly
#     $cb->grid(-sticky => 'w');
#     # now lay out the fields
#     $select_tab->Label(-text => 'X', -font => $small_bold_font)->grid($controls[0], $controls[1], -sticky => 'e');
#     $select_tab->Label(-text => 'Y', -font => $small_bold_font)->grid($controls[2], $controls[3], -sticky => 'e');
#     $select_tab->Label(-text => 'Z', -font => $small_bold_font)->grid($controls[4], $controls[5], -sticky => 'e');
# 
#     # create a toggled menubutton for the material list
#     my $tm = togglemenu($select_tab, "Material", \%select_material, \&printargs, qw(Mat1 Mat2 Mat3));
#     $tm->configure(-state => 'disabled');
#     # create the checkbutton which enables/disables the menu
#     $select_tab->Checkbutton(
#         -text => 'Material',
#         -variable => \$select_material_flag,
#         -command => [ \&control_switcher, \$select_material_flag, $tm ],
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid($tm, "-");
# 
#     # create a big 'select all' button
#     $select_tab->Button(
#         -text => 'Select All',
#         -font => $small_bold_font,
#         -command => sub { add_to_current_group(@things) },
#         )->grid('-','-');
# 
#     # 'show' settings
# 
#     my $show_tab = $toolbox_nb->add('show', -label => 'Show');
# 
#     # create the position filter fields first so the checkbutton can turn them on & off
#     @controls = ();
#     foreach (0 .. 2) {
#         push @controls, $show_tab->NumEntry(
#             -textvariable => \$show_filter_pos_min[$_],
#             -state => 'disabled',
#             -browsecmd => \&queue_gl_draw,
#             -command => \&queue_gl_draw,
#             %{ $default_widget_type_properties->{NumEntry} },
#             );
#     }
#     foreach (0 .. 2) {
#         push @controls, $show_tab->NumEntry(
#             -textvariable => \$show_filter_pos_max[$_],
#             -state => 'disabled',
#             -browsecmd => \&queue_gl_draw,
#             -command => \&queue_gl_draw,
#             %{ $default_widget_type_properties->{NumEntry} },
#             );
#     }
# 
#     push @controls, $show_tab->Checkbutton(
#         -text => 'Filter GL view',
#         -variable => \$filter_gl_view,
#         -command => \&queue_gl_draw,
#         %{ $default_widget_type_properties->{Checkbutton} },
#         );
# 
#     push @controls, $show_tab->Checkbutton(
#         -text => 'Include object dimension',
#         -variable => \$show_filter_includes_dimension,
#         -command => \&queue_gl_draw,
#         %{ $default_widget_type_properties->{Checkbutton} },
#         );
# 
#     push @controls, $show_tab->Checkbutton(
#         -text => 'Filter by intersection',
#         -variable => \$filter_by_intersection,
#         -command => \&queue_gl_draw,
#         %{ $default_widget_type_properties->{Checkbutton} },
#         );
# 
#     # create the checkbutton which enables/disables the fields
#     $show_tab->Checkbutton(
#         -text => 'Position',
#         -variable => \$filter_shown_by_position,
#         -command => [ \&control_switcher, \$filter_shown_by_position, @controls ],
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid(
#             $show_tab->Label(-text => 'Min', -font => $small_bold_font),
#             $show_tab->Label(-text => 'Max', -font => $small_bold_font),
#             );
#     # now lay out the fields
#     $show_tab->Label(-text => 'X', -font => $small_bold_font)->grid($controls[0], $controls[3], -sticky => 'e');
#     $show_tab->Label(-text => 'Y', -font => $small_bold_font)->grid($controls[1], $controls[4], -sticky => 'e');
#     $show_tab->Label(-text => 'Z', -font => $small_bold_font)->grid($controls[2], $controls[5], -sticky => 'e');
#     $controls[6]->grid();
#     $controls[7]->grid();
#     $controls[8]->grid();
# 
#     # 'gl' settings
# 
#     my $opengl_tab = $toolbox_nb->add('opengl', -label => 'OpenGL');
# 
#     $opengl_tab->Checkbutton(
#         -text => 'Up vector above Eye',
#         -variable => \$gl_setting->{up_vector_above_eye},
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid('-','-','-', -sticky => 'w');
# 
#     $opengl_tab->Checkbutton(
#         -text => 'Eye follows cursor',
#         -variable => \$gl_setting->{eye_follows_cursor},
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid('-','-','-', -sticky => 'w');
# 
#     $opengl_tab->Checkbutton(
#         -text => 'Light follows cursor',
#         -variable => \$gl_setting->{light_follows_cursor},
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid('-','-','-', -sticky => 'w');
# 
#     $opengl_tab->Checkbutton(
#         -text => 'Target follows cursor',
#         -variable => \$gl_setting->{target_follows_cursor},
#         %{ $default_widget_type_properties->{Checkbutton} },
#         )->grid('-','-','-', -sticky => 'w');
# 
#     # XYZ vector OpenGL settings
#     foreach my $setting (keys %{ $gl_var }) {
#         @controls = ();
#         foreach my $element (sort keys %{ $gl_var->{$setting} }) {
#             push @controls, $opengl_tab->NumEntry(
#                 -textvariable => \$gl_var->{$setting}->{$element},
#                 -browsecmd => \&queue_gl_draw,
#                 -command => \&queue_gl_draw,
#                 -increment => $gl_increment->{$setting}->{$element},
#                 %{ $default_widget_type_properties->{NumEntry} },
#                 );
#         } 
#         my $label = ucfirst $setting . " (" . join(',', map(ucfirst, sort keys %{ $gl_var->{$setting} })) . ")";
#         $opengl_tab->Label(-text => $label, -font => $small_bold_font)->grid('-', '-', -sticky => 'w');
#         my $first = shift @controls;
#         $first->grid(@controls);
#     }
# 
#     # rendering selected items for GL view
#     # these controls change the effect of selection rather the selection itself, but the selection tab is still the best place for them
#     $select_tab->Label(-text => "3D View", -font => $small_bold_font)->grid(
#         $select_tab->Checkbutton(
#             -text => 'Render',
#             -variable => \$gl_setting->{render_by_selection},
#             -command => [ \&queue_gl_draw ],
#             %{ $default_widget_type_properties->{Checkbutton} },
#             ),
#         $select_tab->Checkbutton(
#             -text => 'Invert',
#             -variable => \$gl_setting->{render_unselected},
#             -command => [ \&queue_gl_draw ],
#             %{ $default_widget_type_properties->{Checkbutton} },
#             ),
#     );
# 
#     # create initial views
#     $flat_view{XY}->{canvas} = $view_frame->Canvas( -background => 'darkgrey', -takefocus => 0, -cursor => 'dotbox')->grid(
#         $flat_view{YZ}->{canvas} = $view_frame->Canvas( -background => 'darkgrey', -takefocus => 0, -cursor => 'dotbox'),
#         -sticky => 'nsew');
#     $flat_view{XZ}->{canvas} = $view_frame->Canvas( -background => 'darkgrey', -takefocus => 0, -cursor => 'dotbox')->grid(
#         $gl_frame = $view_frame->Frame(-height => 20, -width => 20, -background => 'red'),
#         -sticky => 'nsew');
# 
#     # make the views autosize
#     my ($columns, $rows) = $view_frame->gridSize();
#     for (my $i = 0; $i < $columns; $i++) {
#         $view_frame->gridColumnconfigure($i, -weight => 1);
#     }
#     for (my $i = 0; $i < $rows; $i++) {
#         $view_frame->gridRowconfigure($i, -weight => 1);
#     }
# 
#     # do work for each view
#     foreach my $view_name (keys %flat_view) {
#         my $view = $flat_view{$view_name};
#         $view->{title} = $view_name;
#         my $canvas = $view->{canvas};
# 
#         # remove default bindings (mouse wheel scrolls up & down)
#         $canvas->bindtags(undef);
# 
#         # map from canvas to view
#         $canvas_view{$canvas} = $view;
# 
#         # create a rectangle to use when selecting multiple objects
#         $view->{selection_box_id} = $canvas->createRectangle(0,0,100,100,
#             -state => 'hidden', -tags => 'selection', -outline => 'SpringGreen2'),
# 
#         # selection lines
#         $view->{selection_line_x} = $canvas->createLine(0,0,0,0,
#             -tags => 'selection',
#             -fill => 'SpringGreen2'),
#         $view->{selection_line_y} = $canvas->createLine(0,0,0,0,
#             -tags => 'selection',
#             -fill => 'SpringGreen2'),
# 
#         # selected lines
#         $view->{selected_line_x} = $canvas->createLine(0,0,0,0,
#             -tags => 'selection',
#             -fill => 'gold'),
#         $view->{selected_line_y} = $canvas->createLine(0,0,0,0,
#             -tags => 'selection',
#             -fill => 'gold'),
# 
#         # new object
#         $view->{new_object_indicator} = $canvas->createRectangle(0,0,0,0,
#             -tags => 'selection',
#             -outline => 'green'),
# 
#         # mouse-wheel moves selection up & down
#         $canvas->CanvasBind("<ButtonPress-4>", [ \&zoom_tracker, -1 ] );
#         $canvas->CanvasBind("<ButtonPress-5>", [ \&zoom_tracker, 1 ] );
# 
#         # select thing or start multiple selection
#         $canvas->CanvasBind("<ButtonPress-1>", [ \&click_tracker, 0, Ev('x'), Ev('y') ] );
#         $canvas->CanvasBind("<ButtonRelease-1>", [ \&click_tracker, 1, Ev('x'), Ev('y') ] );
# #        $canvas->CanvasBind("<ButtonPress-2>", [ \&click_tracker, 2, Ev('x'), Ev('y') ] );
#         $canvas->CanvasBind("<ButtonPress-3>", [ \&undo_last_click, 3, Ev('x'), Ev('y') ] );
# 
#         # move selected thing or continue multiple selection 
#         $canvas->CanvasBind("<Button1-Motion>", [ \&drag_tracker, 1, Ev('x'), Ev('y') ] );
#         # finish multiple selection
# #        $canvas->CanvasBind("<ButtonRelease-1>", [ \&drag_tracker, 0, Ev('x'), Ev('y') ] );
# 
#         $canvas->CanvasBind("<Motion>", [ \&move_tracker, Ev('x'), Ev('y'), {} ] );
# 
#         # these controls change the effect of selection rather the selection itself, but the selection tab is still the best place for them
#         $view->{render_by_selection} = 0;
#         $view->{render_unselected} = 0;
#         $select_tab->Label(-text => "View $view_name", -font => $small_bold_font)->grid(
#             $select_tab->Checkbutton(
#                 -text => 'Control Render',
#                 -variable => \$view->{render_by_selection},
#                 -command => [ \&refresh_views ],
#                 %{ $default_widget_type_properties->{Checkbutton} },
#                 ),
#             $select_tab->Checkbutton(
#                 -text => 'Invert',
#                 -variable => \$view->{render_unselected},
#                 -command => [ \&refresh_views ],
#                 %{ $default_widget_type_properties->{Checkbutton} },
#                 ),
#         );
# 
#     }
# 
#     $main_win->bind("<KeyPress-Shift_L>" => sub { $shift_key_down |= 0x01; $log->debug("shift_key_down now $shift_key_down\n") if $debug; } );
#     $main_win->bind("<KeyPress-Shift_R>" => sub { $shift_key_down |= 0x02; $log->debug("shift_key_down now $shift_key_down\n") if $debug; } );
#     $main_win->bind("<KeyRelease-Shift_L>" => sub { $shift_key_down &= ~0x01; $log->debug("shift_key_down now $shift_key_down\n") if $debug; } );
#     $main_win->bind("<KeyRelease-Shift_R>" => sub { $shift_key_down &= ~0x02; $log->debug("shift_key_down now $shift_key_down\n") if $debug; } );
# 
#     $main_win->bind("<KeyPress-Control_L>" => sub { $control_key_down |= 0x01; $log->debug("control_key_down now $control_key_down\n") if $debug; } );
#     $main_win->bind("<KeyPress-Control_R>" => sub { $control_key_down |= 0x02; $log->debug("control_key_down now $control_key_down\n") if $debug; } );
#     $main_win->bind("<KeyRelease-Control_L>" => sub { $control_key_down &= ~0x01; $log->debug("control_key_down now $control_key_down\n") if $debug; } );
#     $main_win->bind("<KeyRelease-Control_R>" => sub { $control_key_down &= ~0x02; $log->debug("control_key_down now $control_key_down\n") if $debug; } );
# 
#     $main_win->bind("<KeyPress-Alt_L>" => sub { $alt_key_down |= 0x01; $log->debug("alt_key_down now $alt_key_down\n") if $debug; } );
#     $main_win->bind("<KeyPress-Alt_R>" => sub { $alt_key_down |= 0x02; $log->debug("alt_key_down now $alt_key_down\n") if $debug; } );
#     $main_win->bind("<KeyRelease-Alt_L>" => sub { $alt_key_down &= ~0x01; $log->debug("alt_key_down now $alt_key_down\n") if $debug; } );
#     $main_win->bind("<KeyRelease-Alt_R>" => sub { $alt_key_down &= ~0x02; $log->debug("alt_key_down now $alt_key_down\n") if $debug; } );
# 
#     $main_win->update;
#     $status_entry->focus;
# 
#     return;
# }
# 
################################################################################

1;
