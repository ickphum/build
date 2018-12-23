use strict;
package WxBuildViewPanel;

use Wx qw[:everything];
use Log::Log4perl qw(get_logger);
use base qw(Wx::Panel);

use WxBuildCanvas;
use WxBuildSpinCtrl;

my $log = get_logger();

sub new {
	my( $class, $parent, $id, $pos, $size, $style, $name ) = @_;
	$parent = undef              unless defined $parent;
	$id     = -1                 unless defined $id;
	$pos    = wxDefaultPosition  unless defined $pos;
	$size   = wxDefaultSize      unless defined $size;
    $style  = 0                  unless defined $style;
	$name   = ""                 unless defined $name;

	my $self = $class->SUPER::new( $parent, $id, $pos, $size, $style, $name );

    # ortho controls
	$self->{eye_x_spc} = WxBuildSpinCtrl->new($self, -1, "100", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -1000, 1000, 99);
	$self->{eye_y_spc} = WxBuildSpinCtrl->new($self, -1, "100", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -1000, 1000, 99);
	$self->{eye_z_spc} = WxBuildSpinCtrl->new($self, -1, "100", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -1000, 1000, 99);
	$self->{eye_to_origin_btn} = Wx::BitmapButton->new($self, -1, Wx::Bitmap->new("images/eye_origin.png", wxBITMAP_TYPE_ANY));
	$self->{axes_to_origin_btn} = Wx::BitmapButton->new($self, -1, Wx::Bitmap->new("images/axes_origin.png", wxBITMAP_TYPE_ANY));
	$self->{eye_to_origin_btn}->SetToolTip("Reset the view's eye to the default");

    # perspective controls
	$self->{rotate_around_x_btn} = Wx::BitmapButton->new($self, -1, Wx::Bitmap->new("images/rotate_around_x.png", wxBITMAP_TYPE_ANY));
	$self->{rotate_around_y_btn} = Wx::BitmapButton->new($self, -1, Wx::Bitmap->new("images/rotate_around_y.png", wxBITMAP_TYPE_ANY));
	$self->{rotate_around_z_btn} = Wx::BitmapButton->new($self, -1, Wx::Bitmap->new("images/rotate_around_z.png", wxBITMAP_TYPE_ANY));
	$self->{perspective_angle_spc} = WxBuildSpinCtrl->new($self, -1, "58", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 1, 360, 59);
	$self->{perspective_near_spc} = WxBuildSpinCtrl->new($self, -1, "2", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1000, 3);
	$self->{perspective_far_spc} = WxBuildSpinCtrl->new($self, -1, "1001", wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 1000, 1002);

	$self->{canvas} = WxBuildCanvas->new($self, -1);

	$self->__set_properties();
	$self->__do_layout();

	return $self;

}


sub __set_properties {
	my $self = shift;

    for my $name (qw(eye_x eye_y eye_z perspective_angle perspective_near perspective_far)) {
        $self->{"${name}_spc"}->SetVar($name);
    }

}

sub __do_layout {
	my $self = shift;

	$self->{sizer_1} = Wx::BoxSizer->new(wxVERTICAL);

    # ortho controls
	$self->{ortho_sizer} = Wx::BoxSizer->new(wxHORIZONTAL);
	$self->{ortho_sizer}->Add($self->{eye_x_spc}, 0, 0, 0);
	$self->{ortho_sizer}->Add($self->{eye_y_spc}, 0, 0, 0);
	$self->{ortho_sizer}->Add($self->{eye_z_spc}, 0, 0, 0);
	$self->{ortho_sizer}->Add($self->{eye_to_origin_btn}, 0, 0, 0);
	$self->{ortho_sizer}->Add($self->{axes_to_origin_btn}, 0, 0, 0);
	$self->{sizer_1}->Add($self->{ortho_sizer}, 0, wxEXPAND, 0);

    # perspective controls
	$self->{perspective_sizer} = Wx::BoxSizer->new(wxHORIZONTAL);
	$self->{perspective_sizer}->Add($self->{rotate_around_x_btn}, 0, 0, 0);
	$self->{perspective_sizer}->Add($self->{rotate_around_y_btn}, 0, 0, 0);
	$self->{perspective_sizer}->Add($self->{rotate_around_z_btn}, 0, 0, 0);
	$self->{perspective_sizer}->Add($self->{perspective_angle_spc}, 0, 0, 0);
	$self->{perspective_sizer}->Add($self->{perspective_near_spc}, 0, 0, 0);
	$self->{perspective_sizer}->Add($self->{perspective_far_spc}, 0, 0, 0);
	$self->{sizer_1}->Add($self->{perspective_sizer}, 0, wxEXPAND, 0);

    # canvas
	$self->{sizer_1}->Add($self->{canvas}, 1, wxEXPAND, 0);

	$self->SetSizer($self->{sizer_1});

    # all controls hidden initially
    $self->{sizer_1}->Hide(0);
    $self->{sizer_1}->Hide(1);

	$self->Layout();

}

sub toggle_controls {
    my ($self) = @_;
    $log->debug("toggle_controls 1");

    return unless defined $self->{canvas}->ortho;
    # this is not trustworthy until we're fully initialised but this function
    # is only called on user input
    my $control_index = $self->{canvas}->ortho ? 0 : 1;

    my $control_sizer = $self->{sizer_1}->GetItem($control_index);
    if ($control_sizer->IsShown) {
        $self->{sizer_1}->Hide($control_index);
    }
    else {
        $self->{sizer_1}->Show($control_index);
    }
	$self->Layout();
    $log->debug("toggle_controls 2");

}


1;

