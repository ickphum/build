package LabelSlider; # {{{1

use Wx qw[:slider :sizer :misc];
use base qw(Wx::Panel);
use strict;

use Data::Dumper;

sub new { # {{{2
    my ($class, $parent, $id, $value, $minValue, $maxValue, $stub, $size) = @_;

    my $self = $class->SUPER::new( $parent, $id, );
    $self->{slider} = Wx::Slider->new ($self, $id, $value , $minValue, $maxValue, wxDefaultPosition, [ 40, -1 ]);
    $self->{label} = Wx::StaticText->new ($self, $id, "$stub $value");
    $self->{stub} = $stub;
    $self->{sizer} = Wx::BoxSizer->new(wxVERTICAL);
    $self->{sizer}->Add($self->{label}, 0,wxEXPAND,0);
    $self->{sizer}->Add($self->{slider}, 0,wxEXPAND,0);
    $self->SetSizer($self->{sizer});
    $self->{sizer}->Fit($self);
    $self->Layout;

    return $self;
}

################################################################################
sub event_control { # {{{2
    my ($self) = @_;

    return $self->{slider};
}

################################################################################
sub GetValue { # {{{2
    my ($self, @args) = @_;

    my $value = $self->{slider}->GetValue(@args);
    $self->{label}->SetLabel("$self->{stub} $value");
    return $value;
}

################################################################################
sub SetValue { # {{{2
    my ($self, @args) = @_;

    $self->{label}->SetLabel("$self->{stub} $args[0]");
    return $self->{slider}->SetValue(@args);
}

################################################################################
1;
