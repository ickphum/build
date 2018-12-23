package VectorFrame;

use Wx qw[:frame :sizer :misc :id];
use base qw(Wx::Frame);
use strict;

use Data::Dumper;
use Log::Log4perl qw(get_logger);

use FloatSpinCtrl;

my $log = get_logger;

sub new { # {{{2
    my ($class, $parent, $id, $vector, $increment, $callback, $value_mode_flag ) = @_;

    my @pos = $parent->GetPositionXY;
    my $self = $class->SUPER::new($parent, $id, 'Vector', [ $pos[0] + 5, $pos[1] + 500], [ 100, 200],
        wxFRAME_TOOL_WINDOW | wxCAPTION);

    $self->{ok_btn} = Wx::Button->new ($self, wxID_OK, '');
    Wx::Event::EVT_BUTTON($self, $self->{ok_btn}, sub { my ($self, $event) = @_; $self->Hide; });

    $self->{sizer} = Wx::BoxSizer->new(wxVERTICAL);
    for my $dim (0 .. 2) {
        $self->{dim_fsc}->[$dim] = FloatSpinCtrl->new ($self, -1, $vector->[$dim], $increment, undef, undef, $value_mode_flag);
        Wx::Event::EVT_TEXT($self, $self->{dim_fsc}->[$dim]->event_control, 
            sub {
                my ($self, $event) = @_;

                # get the value from the FloatSpinCtrl object, not the text ctrl itself (increment mode)
                $self->{vector}->[$dim] = $event->GetEventObject->GetParent->GetValue;

                if ($callback) {
                    $callback->($self->{vector}, $dim);
                }
            });
        $self->{sizer}->Add($self->{dim_fsc}->[$dim], 0,wxEXPAND,0);
    }
    $self->{sizer}->Add($self->{ok_btn}, 0,wxEXPAND,0);
    $self->SetSizer($self->{sizer});
    $self->{sizer}->Fit($self);

    $self->{vector} = $vector;

    $self->Layout;
    $self->Show;
    $self->Raise;

    return $self;
}

################################################################################
sub set_vector { # {{{2
    my ($self, $vector, $increment, $value_mode_flag) = @_;

    $self->{vector} = $vector;
    for my $dim (0 .. 2) {
        $self->{dim_fsc}->[$dim]->SetValue($vector->[$dim]);
        if (defined $increment) {
            $self->{dim_fsc}->[$dim]->set_increment($increment);
        }
        if (defined $value_mode_flag) {
            $self->{dim_fsc}->[$dim]->set_value_mode($value_mode_flag);
        }
    }

    $self->Show;

    return;
}

################################################################################
1;
