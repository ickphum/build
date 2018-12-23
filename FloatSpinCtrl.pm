package FloatSpinCtrl;

use Wx qw[:spinbutton :sizer :misc];
use base qw(Wx::Panel);
use strict;

use Data::Dumper;
use Log::Log4perl qw(get_logger);

my $log = get_logger;

# value display modes
my $VM_INCREMENT = 1;
my $VM_VALUE = 2;

sub new { # {{{2
    my ($class, $parent, $id, $value, $increment, $min_value, $max_value, $value_mode_flag) = @_;
    $min_value = -1000 unless defined $min_value;
    $max_value = 1000 unless defined $max_value;
    $value_mode_flag = 1 unless defined $value_mode_flag;

    my $self = $class->SUPER::new( $parent, $id, );
    $self->{spinbutton} = Wx::SpinButton->new ($self, $id, wxDefaultPosition, wxDefaultSize, 
        wxSP_HORIZONTAL );

    # can't turn off range checking...
    $self->{spinbutton}->SetRange(-1, 1);
    $self->{spinbutton}->SetValue(0);
    $self->{value_txt} = Wx::TextCtrl->new ($self, $id, $value, wxDefaultPosition, [ 40, -1 ]);
    $self->{increment} = $increment;
    $self->{min_value} = $min_value;
    $self->{max_value} = $max_value;
    $self->{sizer} = Wx::BoxSizer->new(wxHORIZONTAL);
    $self->{sizer}->Add($self->{value_txt}, 1,wxEXPAND,0);
    $self->{sizer}->Add($self->{spinbutton}, 0,wxEXPAND,0);

    $self->{orig_value} = $self->{value} = $value;

    $self->set_value_mode($value_mode_flag);

    # We're tracking our own min/max limits since spin buttons can't handle floats.
    # We use a range of -1,1 for the spin button so we can use the button's value
    # (which is unrelated to the float value) to control the state of the buttons.
    # Basically, every up or down click away from the range ends gets vetoed, so
    # the spin button value remains at 0 and both buttons are enabled. A click which 
    # reaches a range doesn't get vetoed, the spin button value goes to 1 or -1, and
    # the appropriate button is disabled. The next click must come from the other button;
    # we recognise this situation by the GetPosition method of the event, which returns
    # the new value of the button; if this is 0, it's the click away from the range end, 
    # which should also not get vetoed, so the spin button value returns to 0 and both
    # buttons are enabled.

    Wx::Event::EVT_SPIN_UP($self, $self->{spinbutton},
        sub {
            my ($self, $event) = @_;
            my $value = $self->{value};
#            $log->debug("current value $value");
            $value += $self->{increment};
#            $log->debug("inc'ed value $value");
            if ($value >= $self->{max_value}) {

                # reached max value, skip to disable up button
                $event->Skip;
                $value = $self->{max_value};
            }
            else {
                if (my $pos = $event->GetPosition) {

                    # value ok, pos would be non-zero; veto to keep up button enabled
                    $event->Veto;
                }
                else {

                    # value ok, pos would be zero; skip to go to zero and re-enable down
                    $event->Skip;
                }
            }
            $self->{value} = $value;
            if ($self->{mode} == $VM_INCREMENT) {
                $value = $value - $self->{orig_value};
#                $log->debug("increment from $self->{orig_value} = $value");
            }
            $self->{value_txt}->SetValue($value);
        });

    Wx::Event::EVT_SPIN_DOWN($self, $self->{spinbutton},
        sub {
            my ($self, $event) = @_;
            my $value = $self->{value};
            $value -= $self->{increment};
            if ($value <= $self->{min_value}) {
                $value = $self->{min_value};
                $event->Skip;
            }
            else {
                if (my $pos = $event->GetPosition) {
                    $event->Veto;
                }
                else {
                    $event->Skip;
                }
            }
            $self->{value} = $value;
            if ($self->{mode} == $VM_INCREMENT) {
                $value = $value - $self->{orig_value};
            }
            $self->{value_txt}->SetValue($value);
        });

    # this event will fire on spin button SetValues and on text entry
    Wx::Event::EVT_TEXT($self, $self->{value_txt}, 
        sub {
            my ($self, $event) = @_;

            # in increment mode, test the real value, not the display value
            my $value = ($self->{mode} == $VM_VALUE)
                ? $event->GetEventObject->GetValue
                : $self->{value};

            if ($value < $self->{min_value}) {
                $event->GetEventObject->ChangeValue($self->{min_value});
            }
            elsif ($value > $self->{max_value}) {
                $event->GetEventObject->ChangeValue($self->{max_value});
            }
            else {
                $event->Skip;
            }
        });

    # switch modes on double-click
    Wx::Event::EVT_LEFT_DCLICK($self->{value_txt},
        sub { 
            my ($text_ctrl, $event) = @_;
            my $fsc = $text_ctrl->GetParent;
            $fsc->set_value_mode($fsc->{mode} == $VM_INCREMENT);
        });

    $self->SetSizer($self->{sizer});
    $self->{sizer}->Fit($self);
    $self->Layout;

    return $self;
}

################################################################################
sub set_increment { # {{{2
    my ($self, $increment) = @_;

    return $self->{increment} = $increment;
}

################################################################################
sub set_value_mode { # {{{2
    my ($self, $value_mode_flag) = @_;

    if ($value_mode_flag) {
        $self->{mode} = $VM_VALUE;
        $self->{value_txt}->ChangeValue($self->{value});
        $self->{value_txt}->SetEditable(1);
        $self->{value_txt}->SetBackgroundColour(Wx::Colour->new('white'));
    }
    else {
        $self->{mode} = $VM_INCREMENT;
        $self->{value_txt}->ChangeValue($self->{value} - $self->{orig_value});
        $self->{value_txt}->SetEditable(0);
        $self->{value_txt}->SetBackgroundColour(Wx::Colour->new('yellow'));
    }

    return;
}

################################################################################
sub event_control { # {{{2
    my ($self) = @_;

    return $self->{value_txt};
}

################################################################################
sub GetValue { # {{{2
    my ($self) = @_;

    my $value = ($self->{mode} == $VM_VALUE)
        ? $self->{value_txt}->GetValue
        : $self->{value};

    return $value;
}

################################################################################
sub SetValue { # {{{2
    my ($self, $value) = @_;

    # a new value resets the original_value used for increment mode;
    # there is no way to change the current increment at present.
    $self->{orig_value} = $self->{value} = $value;

    # easy way to set state & value
    $self->set_value_mode($self->{mode} == $VM_VALUE);

    return;
}

################################################################################
1;
