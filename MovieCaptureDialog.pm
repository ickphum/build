# generated by wxGlade 0.6.3 on Mon Jan 12 22:26:22 2009
# To get wxPerl visit http://wxPerl.sourceforge.net/

use Wx 0.15 qw[:allclasses];
use strict;
package MovieCaptureDialog;

use Wx qw[:everything];
use base qw(Wx::Dialog);
use strict;
use Log::Log4perl qw(get_logger);
my $log = get_logger;

# begin wxGlade: ::dependencies
# end wxGlade

sub new {
	my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;
	$parent = undef              unless defined $parent;
	$id     = -1                 unless defined $id;
	$title  = ""                 unless defined $title;
	$pos    = wxDefaultPosition  unless defined $pos;
	$size   = wxDefaultSize      unless defined $size;
	$name   = ""                 unless defined $name;

# begin wxGlade: MovieCaptureDialog::new

	$style = wxDEFAULT_DIALOG_STYLE 
		unless defined $style;

	$self = $self->SUPER::new( $parent, $id, $title, $pos, $size, $style, $name );
	$self->{keep_the_movie_cbx} = Wx::CheckBox->new($self, -1, "Keep the movie", wxDefaultPosition, wxDefaultSize, );
	$self->{remove_image_file_cbx} = Wx::CheckBox->new($self, -1, "Remove image files", wxDefaultPosition, wxDefaultSize, );
	$self->{label_1} = Wx::StaticText->new($self, -1, "Movie file name:", wxDefaultPosition, wxDefaultSize, );
	$self->{movie_file_name_txt} = Wx::TextCtrl->new($self, -1, "", wxDefaultPosition, wxDefaultSize, );
	$self->{bitmap_button_1} = Wx::BitmapButton->new($self, -1, Wx::Bitmap->new("/home/ikm/subversion/ikm/build/images/icons/mimetypes/video.png", wxBITMAP_TYPE_ANY));
	$self->{Ok} = Wx::Button->new($self, wxID_OK, "");

	$self->__set_properties();
	$self->__do_layout();

	Wx::Event::EVT_BUTTON($self, $self->{bitmap_button_1}->GetId, \&show_file_dialog);

# end wxGlade
	return $self;

}

sub init {
    my ($self, $filename) = @_;
    $self->{movie_file_name_txt}->SetValue($filename);

    return;
}

sub values {
    my ($self) = @_;

    return $self->{keep_the_movie_cbx}->IsChecked, $self->{remove_image_file_cbx}->IsChecked, $self->{movie_file_name_txt}->GetValue;
}

sub __set_properties {
	my $self = shift;

# begin wxGlade: MovieCaptureDialog::__set_properties

	$self->SetTitle("Movie Capture");
	$self->SetSize(Wx::Size->new(359, 200));
	$self->{keep_the_movie_cbx}->SetValue(1);
	$self->{remove_image_file_cbx}->SetValue(1);
	$self->{bitmap_button_1}->SetSize($self->{bitmap_button_1}->GetBestSize());

# end wxGlade
}

sub __do_layout {
	my $self = shift;

# begin wxGlade: MovieCaptureDialog::__do_layout

	$self->{sizer_3} = Wx::BoxSizer->new(wxVERTICAL);
	$self->{sizer_4} = Wx::BoxSizer->new(wxHORIZONTAL);
	$self->{sizer_3}->Add($self->{keep_the_movie_cbx}, 0, wxALL, 20);
	$self->{sizer_3}->Add($self->{remove_image_file_cbx}, 0, wxLEFT|wxRIGHT|wxBOTTOM, 20);
	$self->{sizer_3}->Add($self->{label_1}, 0, wxLEFT|wxRIGHT, 20);
	$self->{sizer_4}->Add($self->{movie_file_name_txt}, 1, wxLEFT|wxEXPAND, 20);
	$self->{sizer_4}->Add($self->{bitmap_button_1}, 0, wxRIGHT|wxEXPAND, 20);
	$self->{sizer_3}->Add($self->{sizer_4}, 0, wxEXPAND, 0);
	$self->{sizer_3}->Add($self->{Ok}, 0, wxTOP|wxALIGN_CENTER_HORIZONTAL, 10);
	$self->SetSizer($self->{sizer_3});
	$self->Layout();

# end wxGlade
}


sub show_file_dialog {
	my ($self, $event) = @_;
# wxGlade: MovieCaptureDialog::show_file_dialog <event_handler>
# end wxGlade

    my $dialog = Wx::FileDialog->new($self, 'Choose a file', '', 
        $self->{movie_file_name_txt}->GetValue, '*.*', wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
    if ($dialog->ShowModal == wxID_OK) {
        $self->{movie_file_name_txt}->SetValue($dialog->GetFilename);
    }
    $dialog->Destroy;

	$event->Skip;

    return;
}

# end of class MovieCaptureDialog

1;

