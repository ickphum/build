#$Id: Window.pm 191 2009-06-24 05:16:38Z ikm $

use strict;
package PropFrame;

use Wx qw[:everything];
use base qw(Wx::MiniFrame Class::Accessor::Fast);
use strict;

use Data::Dumper;
use Log::Log4perl qw(get_logger);
use List::MoreUtils;
use Storable qw(dclone);

use lib "..";

use LabelSlider;
use FloatSpinCtrl;
use GridThing;

# shared data 

# private data 

my $log = get_logger;

# accessors {{{1

__PACKAGE__->mk_accessors(qw(notebook complete_list selected_list old_selections));

# methods {{{1

################################################################################
# Constructor. Creates a Wx::Frame object, adds a sizer and a status bar and
# sets the window size.
sub new { #{{{2
    my ($class, $parent, $title, $pos, $size) = @_;

    $log->debug("PropFrame");
    $title  = ""                 unless defined $title;
    $pos    = [ 10, 10 ]         unless defined $pos;
    $size   = [ 100, 100 ]       unless defined $size;

    my $self = $class->SUPER::new( $parent, -1, $title, $pos, $size);

    $self->SetSizer(my $sizer = Wx::BoxSizer->new(wxVERTICAL));

    Wx::Event::EVT_CLOSE($self, \&handle_close);

    $self->notebook(Wx::Notebook->new($self, -1));

    $self->complete_list(Wx::ListBox->new($self->notebook, -1, wxDefaultPosition, wxDefaultSize, [], wxLB_EXTENDED));
    $self->notebook->AddPage($self->complete_list, "All");
    Wx::Event::EVT_LISTBOX($self, $self->complete_list, \&handle_selection);

    $sizer->Add($self->notebook, 1, wxEXPAND, 0 );

    $self->old_selections([]);

    return $self;

}

#*******************************************************************************
sub update_thing_property { #{{{2
    my ($self) = @_;
    $log->debug("utp: @_");
}

#*******************************************************************************
# Update (which may include removing) the tab which shows the common properties
# of all selected things.
sub update_common_tab { #{{{2
    my ($self) = @_;

    $log->debug("update_common_tab: old selections: " . join(',', @{ $self->old_selections }) );
    my $nbr_selections = scalar @{ $self->old_selections };
#    my $nbr_selections = scalar $self->complete_list->GetSelections;
    my $nbr_pages = $self->notebook->GetPageCount;
    my $nbr_req_pages = $nbr_selections + 1 + ($nbr_selections > 1 ? 1 : 0);
    $log->debug("nbr_selections = $nbr_selections, nbr_pages = $nbr_pages, nbr_req_pages = $nbr_req_pages");

    if ($nbr_pages < $nbr_req_pages) {

        # add the tab
        $log->debug("add common tab");

        # create panel for controls and a sizer for the panel
        my $page = Wx::ScrolledWindow->new($self->notebook, -1);
        $page->SetScrollRate(10,10);
        $page->SetSizer(my $sizer = Wx::FlexGridSizer->new(0,2));

        # add the panel to the notebook
        $self->notebook->InsertPage(1, $page, 'Common');

    }
    elsif ($nbr_pages > $nbr_req_pages) {

        # remove the tab and bail
        $log->debug("delete common tab");
        $self->notebook->DeletePage(1);
        return;

    }

    return if $nbr_selections <= 1;

    my $page = $self->notebook->GetPage(1);
    my $sizer = $page->GetSizer;
    $page->DestroyChildren;

    my $app = wxTheApp;

    my $common_values;
    for my $index (@{ $self->old_selections }) {
        my $thing = $app->things->[$index];
        $log->debug("thing at $index = " . Dumper($thing));

        if ($common_values) {

            # not the first record; compare each value in the common list with this thing's value
            # for that key; different or non-existent = remove.
            for my $key (keys %{ $common_values }) {
#                if (! defined $thing->{$key} || $thing->{$key} ne $common_values->{$key}) {
                if (! defined $thing->{$key}) {
                    delete $common_values->{$key};
                }
            }
        }
        else {

            # first record; just copy all attributes
            $common_values = dclone $thing;
        }
    }

    # delete settings we don't want displayed in common tab
    for my $setting (qw(name)) {
        delete $common_values->{$setting};
    }

    # add controls
    $self->create_controls($page, $common_values, [ @{ $self->old_selections } ] );
#    for my $key (sort keys %{ $common_values }) {
#        $sizer->Add(Wx::StaticText->new($page, -1, $key), 0, 0);
#        $sizer->Add(Wx::TextCtrl->new($page, -1, $common_values->{$key}), 0, 0);
#    }
    $sizer->Layout;

    return;
}

#*******************************************************************************
# Create a tab with the thing's properties
sub add_thing_tab { #{{{2
    my ($self, $thing) = @_;

    # create panel for controls and a sizer for the panel
    my $page = Wx::ScrolledWindow->new($self->notebook, -1);
    $page->SetScrollRate(10,10);
    $page->SetSizer(my $sizer = Wx::FlexGridSizer->new(0,2));

    # add controls
    $self->create_controls($page, $thing);

    # add the panel to the notebook
    $self->notebook->AddPage($page, "$thing->{name}");

    return;
}

#*******************************************************************************
# Create controls for a thing's settings (also creates common tab controls)
sub create_controls { #{{{2
    my ($self, $page, $thing, $common_things) = @_;

    my $sizer = $page->GetSizer;

    my $app = wxTheApp;

    my %setting_info = (
        angle => { class => 'LabelSlider', min => 0, max => 360, },
        axis => { class => 'Choice', list => [ qw(X Y Z) ], },
        color_v => { class => 'ColourPickerCtrl', },
        selected_color_v => { class => 'ColourPickerCtrl', },
        direction => { class => 'Choice', list => [ qw(North South East West) ], },
        texture => { class => 'Choice', list => [ 'none', @{ $app->texture_names } ] },
        texture_infotile => { class => 'CheckBox', },
        texture_infofactor => { class => 'SpinCtrl', min => -20, max => 20, },
        name => { class => 'TextCtrl', },
        sides => { class => 'SpinCtrl', min => 3, max => 50, },
        wall => { class => 'SpinCtrl', min => 1, max => 30, },
        smooth => { class => 'CheckBox', },
        steps => { class => 'SpinCtrl', min => 1, max => 1000, },
        legs => { class => 'SpinCtrl', min => 0, max => 90, },
        square => { class => 'CheckBox', },
        orient => { class => 'CheckBox', },
        climb => { class => 'SpinCtrl', min => 0, max => 90, },
        base => { class => 'SpinCtrl', min => 1, max => 20, },
        ratiotop => { class => 'FloatSpinCtrl', min => 0.01, max => 100, increment => 0.01 },
        ratiobottom => { class => 'FloatSpinCtrl', min => 0.01, max => 100, increment => 0.01 },
        shape => { class => 'Choice', list => [ @{ $app->shape_names } ] },
        stretch => { class => 'CheckBox', },
        swapx => { class => 'CheckBox', },
        swapy => { class => 'CheckBox', },
        swaptexture => { class => 'CheckBox', },
        gridcountx => { class => 'SpinCtrl', min => 1, max => 100, },
        gridcounty => { class => 'SpinCtrl', min => 1, max => 100, },
        gridcountz => { class => 'SpinCtrl', min => 1, max => 100, },
        gridspacex => { class => 'SpinCtrl', min => 1, max => 100, },
        gridspacey => { class => 'SpinCtrl', min => 1, max => 100, },
        gridspacez => { class => 'SpinCtrl', min => 1, max => 100, },
        rotatex => { class => 'SpinCtrl', min => 0, max => 360, },
        rotatey => { class => 'SpinCtrl', min => 0, max => 360, },
        rotatez => { class => 'SpinCtrl', min => 0, max => 360, },
        edgex => { class => 'CheckBox', },
        edgey => { class => 'CheckBox', },
        edgez => { class => 'CheckBox', },
        ringradius => { class => 'FloatSpinCtrl', min => 1, max => 100, increment => 1 },
        ringcount => { class => 'SpinCtrl', min => 1, max => 100, },
        ringstart => { class => 'SpinCtrl', min => 0, max => 360, },
        ringangle => { class => 'SpinCtrl', min => 0, max => 360, },
        ringstretchx => { class => 'SpinCtrl', min => 1, max => 10, },
        ringstretchy => { class => 'SpinCtrl', min => 1, max => 10, },
        centred_on => { class => 'TextCtrl', },
        centred_position => { class => 'Choice', list => [ qw/Radius InnerEdge OuterEdge/ ] },
    );

    # this is different to the list in GridFrame because the compound properties (grid, ring, swap, etc) are expanded here
    my @settings = qw(name dim_v offset_v rotatex rotatey rotatez color_v selected_color_v texture texture_infotile texture_infofactor smooth sides axis angle steps direction
        ratiotop ratiobottom
        wall legs base square 
        gridcountx gridspacex gridcounty gridspacey gridcountz gridspacez
        edgex edgey edgez
        ringradius ringcount ringstart ringangle ringstretchx ringstretchy
        centred_on centred_position
        orient climb
        shape stretch swapx swapy swaptexture
        );

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

    for my $property (@settings) {
        if (defined $thing->{$property} && (my $spec = $setting_info{$property})) {

            my $control;
            if ($spec->{class} eq 'LabelSlider') {
                $control = LabelSlider->new($page, -1, $thing->{$property}, $spec->{min}, $spec->{max}, $spec->{stub} || $property,);
            }
            elsif ($spec->{class} eq 'Choice') {
                $control = Wx::Choice->new($page, -1, wxDefaultPosition, wxDefaultSize, $spec->{list} );
                main::set_control_value($control, $property, $thing->{$property});
            }
            elsif ($spec->{class} eq 'ColourPickerCtrl') {
                $control = Wx::ColourPickerCtrl->new( $page, -1, Wx::Colour->new( map { $_ * 255 } @{ $thing->{$property} } ) );
            }
            elsif ($spec->{class} eq 'TextCtrl') {
                $control = Wx::TextCtrl->new($page, -1, $thing->{$property}, );
            }
            elsif ($spec->{class} eq 'SpinCtrl') {
                $control = Wx::SpinCtrl->new($page, -1, $thing->{$property}, wxDefaultPosition, [ 40, -1 ], wxSP_ARROW_KEYS, 
                    $spec->{min}, $spec->{max} );
            }
            elsif ($spec->{class} eq 'FloatSpinCtrl') {
                $control = FloatSpinCtrl->new($page, -1, $thing->{$property}, $spec->{increment}, $spec->{min}, $spec->{max}, );
            }
            elsif ($spec->{class} eq 'CheckBox') {
                $control = Wx::CheckBox->new($page, -1, $spec->{label} || $property );
                $control->SetValue($thing->{$property});
            }

            # $control->SetToolTip($property);
            $control->SetName($property);
            $sizer->Add(Wx::StaticText->new($page, -1, $property), 0, wxALIGN_CENTER_VERTICAL);
            $sizer->Add($control, 0, wxEXPAND);

            my $control_class = ref $control;

            if (my $event = $control_event{$control_class}) {
                my $event_control = $control_class =~ /Wx::/ ? $control : $control->event_control;
                main::assign_event_handler($event_control, $event, sub {
                    my ($frame, $event) = @_;

                    if ($common_things) {
                        my $value = main::get_control_value($event->GetEventObject, $property);
                        for my $id ( @{ $common_things } ) {
                            my $thing = $app->things->[$id];
                            $thing->$property($value);
                            $thing->calculate_for_render;
                        }
                    }
                    else {
                        $thing->$property(main::get_control_value($event->GetEventObject, $property));
                        $log->debug("new $property:" . $thing->$property);
                        if ($property eq 'name') {

                            # change the tab title as the name changes
                            $self->notebook->SetPageText($self->notebook->GetSelection, $thing->$property);

                            # change the list entry also
                            $self->complete_list->SetString($thing->id, $thing->$property);
                        }
                        $thing->calculate_for_render;
                    }
                });
            }
            else {
                $log->logdie("no event for $control_class");
            }
        }
    }

}

#*******************************************************************************
# Remove the tab with the thing's properties
sub remove_thing_tab { #{{{2
    my ($self, $thing) = @_;

    for my $page_number (0 .. $self->notebook->GetPageCount ) {
        $log->debug("page $page_number = " . $self->notebook->GetPageText($page_number));
        if ($thing->{name} eq $self->notebook->GetPageText($page_number)) {
            $log->debug("DeletePage $page_number");
            $self->notebook->DeletePage($page_number);
            return;
        }
    }

    $log->logdie("didn't find page for $thing->{name}");

    return;
}

#*******************************************************************************
# Handle selection events
sub handle_selection { #{{{2
    my ($self, $event) = @_;

    my $app = wxTheApp;

    my %state;
    my @new_selections = $self->complete_list->GetSelections;
    for my $index (@new_selections) {
        $state{$index} = 1;
    }
    for my $index (@{ $self->old_selections }) {
        $state{$index} ||= 0;
        $state{$index} |= 2;
    }
    $log->debug("new selections: @new_selections, state " . Dumper(\%state));

    for my $index (sort keys %state) {
        if ($state{$index} == 1) {
            $log->debug("select $index");
            $app->things->[$index]->set_state($GridThing::TS_SELECTED_FROM_LIST);
            $self->add_thing_tab($app->things->[$index]);
        }
        elsif ($state{$index} == 2) {
            $log->debug("deselect $index");
            $app->things->[$index]->clear_state($GridThing::TS_SELECTED_FROM_LIST);
            $self->remove_thing_tab($app->things->[$index]);
        }
    }

    $self->old_selections(\@new_selections);
    $log->debug("old selections: " . join(',', @{ $self->old_selections }) );

    $self->update_common_tab;

    return;
}

#*******************************************************************************
# Add a thing
sub add_thing { #{{{2
    my ($self, $thing) = @_;

    $self->complete_list->Append($thing->name);

    return;
}

#*******************************************************************************
# Remove a thing
sub remove_thing { #{{{2
    my ($self, $thing) = @_;

    $self->complete_list->Delete($thing->id);

    return;
}

#*******************************************************************************
# Select (or deselect) a thing due to action in the main window
sub select_thing { #{{{2
    my ($self, $thing, $selected) = @_;

    # select a thing (as opposed to deselect) by default
    $selected = 1 unless defined $selected;

    my $app = wxTheApp;

    my $pos = List::MoreUtils::firstidx { $_->{id} == $thing->{id} } @{ $app->things };
    $log->logdie("can't find $thing->{id}") unless defined $pos;

    if ($selected) {
        $self->complete_list->SetSelection($pos);
        $self->add_thing_tab($thing);
    }
    else {
        $self->complete_list->Deselect($pos);
        $self->remove_thing_tab($thing);
    }

    my @new_selections = $self->complete_list->GetSelections;
    $log->debug("select_thing: new selections: @new_selections");

    # update selection list so we can tell what's changed next time
    $self->old_selections( [@new_selections  ] );

    $self->update_common_tab;

    return;
}

#*******************************************************************************
# Handler for the close event; we hide rather than closing.
sub handle_close { #{{{2
    my ($self, $event) = @_;

    if ($event->CanVeto) {
        $event->Veto();
        $self->Hide;
    }
    else {
        $self->Destroy;
    }

}

1;

__END__

