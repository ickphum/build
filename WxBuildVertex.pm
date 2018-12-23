use strict;
package WxBuildVertex;
use Moose;
use Log::Log4perl qw(get_logger);
use Data::Dumper;

my $log = get_logger();

has 'x' => (
    isa => 'Num',
    is => 'rw',
    default => 0,
);

has 'y' => (
    isa => 'Num',
    is => 'rw',
    default => 0,
);

has 'z' => (
    isa => 'Num',
    is => 'rw',
    default => 0,
);

has 'inc' => (
    isa => 'Num',
    is => 'rw',
    default => 1,
);

__PACKAGE__->meta->make_immutable;

################################################################################

sub vertex {
    my ($self) = @_;

    return $self->x, $self->y, $self->z;
}

################################################################################

sub dim {
    my ($self, $dim, $value) = @_;

    return $dim == 0 
        ? defined($value)
            ? $self->x($value)
            : $self->x
        : $dim == 1
            ? defined($value)
                ? $self->y($value)
                : $self->y
            : $dim == 2
                ? defined($value)
                    ? $self->z($value)
                    : $self->z
                : $log->logdie("bad dim $dim");
}

################################################################################

sub dump {
    my ($self) = @_;

    return '(' . join(',', $self->vertex) . ')';
}

################################################################################

sub set {
    my ($self, $vertex) = @_;

    $self->x($vertex->x);
    $self->y($vertex->y);
    $self->z($vertex->z);
}

################################################################################

sub set_to_origin {
    my ($self) = @_;

    $self->x(0);
    $self->y(0);
    $self->z(0);

    return;
}

################################################################################

sub at_origin {
    my ($self) = @_;

    return ($self->x || $self->y || $self->z) ? 0 : 1;
}

################################################################################

sub equals {
    my ($self, $other) = @_;

    # obviously dodgy with non-exact variables; it will usually be used to check if non-model
    # points (eye, target, etc) are at the origin or at other system-controlled points.
    # We could include a tolerance parameter down the track.
    return $self->x == $other->x &&
        $self->y == $other->y &&
        $self->z == $other->z;
}

################################################################################

1;
