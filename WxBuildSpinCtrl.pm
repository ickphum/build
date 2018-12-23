use strict;
package WxBuildSpinCtrl;

use Wx qw[:everything];
use Log::Log4perl qw(get_logger);
use Data::Dumper;
my $log = get_logger;

use base qw(Wx::SpinCtrl);
use strict;

sub new {
	my( $class, $parent, $id, $default, $position, $size, $style, $min, $max, $initial) = @_;

    # override for standard behaviour
    $size = Wx::Size->new(50, 27);

	my $self = $class->SUPER::new( $parent, $id, $default, $position, $size, $style, $min, $max, $initial) ;

    return $self;
}

################################################################################

sub SetVar {
    my ($self, $varname) = @_;

    $self->SetToolTip(Wx::ToolTip->new($varname));

    $self->{var} = $varname;
    Wx::Event::EVT_SPINCTRL ($self->GetParent, $self, sub { handle_spin_event(@_, $varname); });
    return;
}

################################################################################

sub handle_spin_event {
    my ($parent, $event, @args) = @_;
    my $spin_ctrl = $event->GetEventObject;
#    $log->debug("handle_spin_event: " . $parent->GetName . ", " . $spin_ctrl->GetValue);
    my $view = wxTheApp->{app}->view->{$parent->GetName};
    my $var = $spin_ctrl->{var};
    if ($var =~ /(\w+)_([xyz])\z/) {
        my ($vertex, $coord) = ($1,$2);
        $view->$vertex->$coord($spin_ctrl->GetValue);
    }
    elsif ($var =~ /perspective_(\w+)/) {
        $view->perspective->{$1} = $spin_ctrl->GetValue;
    }
    else {
        $view->$var($spin_ctrl->GetValue);
    }
    $view->canvas->dirty(1);
    return;
}

1;
