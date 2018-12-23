#!/usr/bin/perl -w

# build.pl
#
# thoughts
# 
# sticky mode for building attached items
#
# 
use strict;
use POSIX qw(ceil);
use Time::HiRes qw(usleep);
use Carp;
use Tk;
use Tk::PNG;
use Tk::ROText;
use Tk::TextUndo;
use Tk::NumEntry;
use Tk::LabFrame;
use Tk::NoteBook;
use Tk::Tree;
use Tk::TableMatrix;
use Tk::TableMatrix::Spreadsheet;
use File::Basename;
use Tie::RefHash;
use Getopt::Long;
use Log::Log4perl qw(get_logger);
use FindBin qw($Bin);
use OpenGL;
use OpenGL::Simple::GLUT qw(:all);
use Data::Dumper;
use YAML qw(LoadFile DumpFile);
use List::Util qw(min max);
use List::MoreUtils qw(firstidx mesh);
use Storable qw(dclone);
use Math::Trig;

# standard globals
my $log;
my $debug;
my %options;

my $LINES=0;
my $COLS=0;

my $main_win;
my $toolbox_win;
my $toolbar;
my $control_frame;
my $view_frame;
my $gl_frame;
my $object_list;
my $new_child_menu;
my $new_sibling_menu;
my $source_win;
my $properties_win;
my @property_widgets;
my $source_text;
my $output_text;
my $msg_statusvar = "Builder";
my $status_entry;

# default values allow sensible camera tracking before all views have been visited
my @cursor_statusvar = (10,10,10);
my @selected_statusvar;
my $current_settings_frame;
my $line_win;
my $test_win;
my $line_canvas;

# keep these current via a callback on <Configure>
my $view_width;
my $view_height;

my $small_bold_font = '-*-clean-bold-*-*-*-10-*-*-*-*-*-*-*';
my $small_font      = '-*-clean-medium-*-*-*-10-*-*-*-*-*-*-*';
my $lucida_10_font  = '-*-*-medium-r-*-sans-10-*-*-*-m-*-*-*';

my $current_source;

# templates for object creation
my $default_color = { type => 'color', name => '', color_vector => [ 1, 1, 1, 0, 0 ] };
my %default_object = (
	include => { name => '.inc' },
	declare => { name => 'Var' },
	background => { children => [ $default_color ] },
	light_source => { vector => [ 0,0,0 ], children => [ $default_color ] },

	color => { name => '', color_vector => [ 1,1,1,0,0 ] },
	checker => { children => [ $default_color, $default_color ] },
	hexagon => { children => [ $default_color, $default_color, $default_color ] },

	sphere => { radius => 1, vector => [ 0,0,0 ] },
	box => { vector_1 => [ 0,0,0 ], vector_2 => [ 1,1,1 ] },
	cone => { base_vector => [ 0,0,0 ], base_radius => 2, cap_vector => [ 0,1,0 ], cap_radius => 1 },
	cylinder => { base_vector => [ 0,0,0 ], cap_vector => [ 0,1,0 ], radius => 1 },
	torus => { major_radius => 2, minor_radius => 1 },
	object => { name => 'Var' },
	name => { name => 'Var' },

	plane => { displacement => 0, vector => [ 0,0,0 ] },

	transform => { name => '' },
	rotate => { name => '', vector => [ 0,0,0 ] },
	translate => { name => '', vector => [ 0,0,0 ] },
	scale => { name => '', vector => [ 0,0,0 ] },

	camera => { children => [
		{ type => 'location', vector => [ 0,0,0 ] },
		{ type => 'look_at', vector => [ 0,0,0 ] },
		] },
	location => { vector => [ 0,0,0 ] },
	look_at => { vector => [ 0,0,0 ] },
	angle => { angle => 45 },

	);

my @simple_objects = qw/pigment texture finish union difference merge intersection/;

# default widget properties by type
my $default_widget_type_properties = {
    NumEntry => {
        -width => 4, 
        -font => $small_font,
        -takefocus => 0,
        -foreground => 'black',
        -background => 'white',
    },
    Checkbutton => {
        -font => $small_font,
        -takefocus => 0,
#        -foreground => 'black',
    },
    Radiobutton => {
        -font => $small_bold_font,
        -takefocus => 0,
        -foreground => 'black',
    },
    Button => {
        -font => $small_bold_font,
        -takefocus => 0,
    },
    Text => {
        -font => $small_font,
        -takefocus => 0,
        -background => 'white',
    },
};

# views
my $current_view;
my %flat_view = (
	XY => {
        name => 'XY',
        bg => 'grey',
		size => 100,
		x_dim => 0,
		x_centre => 0, 
        x_axis_direction => -1,
		y_dim => 1,
		y_centre => 0,
        y_axis_direction => 1,
        precision => 2,
        other_dim => 2,
		},
	YZ => {
        name => 'YZ',
        bg => 'darkgrey',
		size => 100,
		x_dim => 2,
		x_centre => 0,
        x_axis_direction => -1,
		y_dim => 1,
		y_centre => 0,
        y_axis_direction => 1,
        precision => 2,
        other_dim => 0,
		},
	XZ => {
        name => 'XZ',
        bg => 'lightgrey',
		size => 100,
		x_dim => 0,
		x_centre => 0,
        x_axis_direction => -1,
		y_dim => 2,
		y_centre => 0,
        y_axis_direction => -1,
        precision => 2,
        other_dim => 1,
		},
	);

# map from canvas to view
my %canvas_view;

# vertices of items under construction; 
my @vertices = ();
my $new_vertex = {};

my @things;
my @system_things;
my $new_thing;

# indexes into @things corresponding with entries in listbox
my @listed_thing_indices;

my @clipboard;
my %path_to_thing = ( '' => { type => 'root', path => '' } );
my $current_path;
my @current_selection;
my @saved_current;
my $GX = my $GY = 32; # keep it square while we're spinning crudely
my $GZ = 64;
# alias for the limits
my %GDIM = ( x => $GX, y => $GY, z => $GZ );
my $shift_key_down = 0;
my $control_key_down = 0;
my $alt_key_down = 0;
my $selection_box_start_x;
my $selection_box_start_y;

# move settings
my $move_order_lb;
my %move_asc_order;
my %move_adjust_flag;
my %move_dim_flag;
my %move_start_amount = ( x => 0, y => 0, z => 0);
my %move_loop_amount = ( x => 0, y => 0, z => 0);

# selection settings
my $select_material_flag;
my %select_material;
my $select_pos_flag;
my @select_pos_var;
my $select_size_flag;
my @select_size_var;

# show filter settings
my $filter_shown_by_position = 0;
my $filter_gl_view = 0;
my $show_filter_includes_dimension = 1;
my $filter_by_intersection = 0;
# these could be initialised to show the entire model when a file is loaded TODO
my @show_filter_pos_min = (-100,-100,-100);
my @show_filter_pos_max = (100,100,100);

# option settings
my $options_settings_frame;
my $drop_unsupported = 1;

# line window
my $add_on_line_click = 1;
my $remove_line_point = 0;
my $smooth_line = 0;
my $splinesteps = 4;
my %line_points = ();
my @line_coords = ();
my $line_id;
my $moving_line_point;

# gl settings
my $gl_settings_frame;
my $gl_var = {
    perspective => {
        angle => 60,
        'close' => 1,
        far => 1000,
    },
    scale => {
        x => 1,
        y => 1,
        z => 1,
    },
    eye => {
        x => 40,
        y => 10,
        z => 80,
    },
    light => {
        x => 20,
        y => 20,
        z => 20,
    },
    target => {
        x => 0,
        y => 0,
        z => 0,
    },
    up => {
        x => 20,
        y => 100,
        z => 40,
    },
};

my $gl_increment = {
    perspective => {
        angle => 5,
        'close' => 0.1,
        far => 100,
    },
#    translate => {
#        x => 0.5,
#        y => 0.5,
#        z => 0.5,
#    },
#    rotate => {
#        x => 0,
#        y => 0,
#        z => 0,
#    },
    scale => {
        x => 0.5,
        y => 0.5,
        z => 0.5,
    },
    eye => {
        x => 0.5,
        y => 0.5,
        z => 0.5,
    },
    light => {
        x => 1,
        y => 1,
        z => 1,
    },
    target => {
        x => 0.5,
        y => 0.5,
        z => 0.5,
    },
    up => {
        x => 0.5,
        y => 0.5,
        z => 0.5,
    },
};

# simple gl settings
my $gl_setting = {
    # automatic "up" vector
    up_vector_above_eye => 1,
    # eye follows cursor
    eye_follows_cursor => 0,
    # light follows cursor
    light_follows_cursor => 0,
    # target follows cursor
    target_follows_cursor => 0,
    # gl rendering controlled by selection
    render_by_selection => 0,
    # gl rendering of unselected rather than selected
    render_unselected => 0,
};

# object types
my ($OT_LINE, $OT_QUAD, $OT_CUBOID, $OT_POLYGON, $OT_PRISM, $OT_RAMP, $OT_STAIRS, )
    = (1 .. 100);

# various option/arg flags

# divisions for point selection
my $divisions = 4;

# snap radius
my $snap_radius = 5;

# text entry modes
my ($TM_NORMAL, $TM_NAME) = (1 .. 100);
my $text_mode = $TM_NORMAL;
my $current_text = "";

# last file loaded or saved
my $filename;

# true if the next drag action defines shown area
my $drag_to_show_flag = 0;

################################################################################

sub
numeric 
{
	$a <=> $b;
}
sub
rev_numeric 
{
	$b <=> $a;
}

################################################################################

sub
update_status(;$)
{
	my $thing = shift || $current_selection[0];

	$msg_statusvar = "$$thing{material} $$thing{id} @ $$thing{x},$$thing{y},$$thing{z}";

    return;
}

################################################################################

# Copied from Math::Geometry to avoid import problems

sub vector_product  {
    my($a,$b,$c,$d,$e,$f)=@_;
    return($b*$f-$c*$e,$c*$d-$a*$f,$a*$e-$b*$d);
}

sub triangle_normal {
    my(($ax,$ay,$az),($bx,$by,$bz),($cx,$cy,$cz))=@_;
    my(@AB)=($bx-$ax,$by-$ay,$bz-$az);
    my(@AC)=($cx-$ax,$cy-$ay,$cz-$az);
    return(vector_product(@AB,@AC));
}

################################################################################

sub
update_listed_thing_indices
{
    @listed_thing_indices = ();
    my $index = 0;
    for my $thing (@things) {
        if ($thing->{listed}) {
            push @listed_thing_indices, $index;
        }
        $index++;
    }
    $log->debug("listed_thing_indices: @listed_thing_indices");
    return;
}

################################################################################

sub
update_selected_flags
{

    # get the list of selected listbox lines and turn that into a list of @thing indices by
    # going through the objects and 
    map { $things[$_]->{_selected} = 0; } (0 .. $#things);
    my @selected_thing_indices = ();
    if (my @selected_lines = $object_list->curselection()) {
        @selected_thing_indices = @listed_thing_indices[@selected_lines];
        for my $i (@selected_thing_indices) {
            $things[$i]->{_selected} = 1;
            if ($things[$i]->{_series_things}) {
                # the series widgets follow the owner widget in the list
                my $nbr_series_widgets = @{ $things[$i]->{_series_things} };
                map { $things[$_]->{selected} = 1; } (($i + 1) .. ($i + $nbr_series_widgets));
                push @selected_thing_indices, (($i + 1) .. ($i + $nbr_series_widgets));
            }
        }
    }

    # return this so we can operate on just the selected things if we want to
    return reverse @selected_thing_indices;
}

################################################################################

sub 
zoom_tracker
{
	# scroll wheel adjusts size
	my ($canvas, $size_inc) = @_;

    my $view = $canvas_view{$canvas};

	if ($control_key_down) {
		$size_inc *= 4;
	}

    $view->{size} += (($view->{size} / 5) * $size_inc);

    recalc_views();
    refresh_views();

	$log->debug("view $view->{name} size $view->{size}\n") if $debug;

}

################################################################################

sub
clear_current_group
{
	foreach (@current_selection) {
		$log->debug("+CCG+ Removing thing $$_{id} from current group\n") if $debug;
		# $canvas->itemconfigure($$_{rect_id}, -state => 'hidden');
		$$_{current} = 0;
	}
	@current_selection = ();
}

################################################################################

sub
remove_from_current_group(@)
{
	foreach my $thing (@_) {
		my $pos = 0;
		foreach (@current_selection) {
			if ($$_{id} == $$thing{id}) {
				$log->debug("Removing thing $$thing{id} from current group\n") if $debug;
#				$canvas->itemconfigure($$_{rect_id}, -state => 'hidden');
				$$thing{current} = 0;
				splice @current_selection, $pos, 1;
				last;
			}
			$pos++;
		}
	}
}

################################################################################

sub
add_to_current_group(@)
{

	$log->debug("Current group now: ", join ",", map("id = $$_{id}", @current_selection), "\n") if $debug;

	foreach my $thing (@_) {
		# check if the thing is in the list already
		# if (scalar grep { $$_{id} == $$thing{id} } @current_selection) {
		if ($$thing{current}) {
			$log->debug("thing $$thing{id} already in list\n") if $debug;
		}
		else {
			# check the selection filters

			# if we're filtering on position, the x,y,z coords for this thing have to be within
			# the ranges defined in @select_pos_var
			next if $select_pos_flag && ($$thing{x} < $select_pos_var[0] || $$thing{x} > $select_pos_var[1]
				|| $$thing{y} < $select_pos_var[2] || $$thing{y} > $select_pos_var[3]
				|| $$thing{z} < $select_pos_var[4] || $$thing{z} > $select_pos_var[5]);

			# if we're filtering on material, the hash value ref'd by the thing's material has to be true
			next if $select_material_flag && ! $select_material{$$thing{material}};

			push @current_selection, $thing;
#			$canvas->itemconfigure($$thing{rect_id}, -state => 'normal');
			$$thing{current} = 1;
			update_status($thing);
			$log->debug("new item $$thing{id} added to selection\n") if $debug;
		}
	}
}

################################################################################

sub 
chooser {
	# chooser sub
	my ($canvas, $x, $y) = @_;

	# find the id of the top item under the cursor
	my $id = ( $canvas->find('withtag', 'current') )[0];
	if (!defined($id) || $control_key_down) {
		# haven't clicked on anything, so reset the current group (maybe) and record x&y for group selection box

		# we can start a drag selection without selecting the thing we click on by
		# holding down ctrl
#		if ($control_key_down) {
#			foreach my $thing (@things) {
#				if ($$thing{id} == $id) {
#					remove_from_current_group($thing);
#					last;
#				}
#			}
#		}

		if ($shift_key_down) {
			@saved_current = (@current_selection);
		}
		else {
			clear_current_group;
		}

		# selection box uses real coords
		$selection_box_start_x = $x;
		$selection_box_start_y = $y;
		$log->debug("No item selected, box start = $selection_box_start_x, $selection_box_start_y\n") if $debug;

		return;
	}

	# set these so the 'up' event from this selection doesn't trigger a selection
	$selection_box_start_x = undef;
	$selection_box_start_y = undef;

	# find our item with that id
	foreach my $thing (@things) {
		if ($$thing{id} == $id) {
			if ($control_key_down) {
				remove_from_current_group($thing);
			}
			else {
				clear_current_group unless $shift_key_down;
				add_to_current_group($thing);
			}
			return;
		}
	}

	$log->debug("chooser didn't find current item $id\n");
}

################################################################################

sub
dump_args
{
	$log->debug("@_\n");
}

################################################################################

sub
select_next_thing()
{
	# make sure there is a thing to choose...
	return if $#things == -1;

	if ($#current_selection == -1) {
		# no current things; choose 1st in main list
		add_to_current_group($things[0]);
	}
	else {
		# one or more current things; move to next (relative to first current thing) in main list.

		# find the first current thing in the main list
		my $pos = 0;
		while ($pos <= $#things && ${$things[$pos]}{id} != ${$current_selection[0]}{id}) {
			$pos++;
		}
		# move pos to next thing
		$pos = $pos == $#things ? 0 : $pos + 1;
		# tabbing from multiple selection clears all of them
		clear_current_group;
		add_to_current_group($things[$pos]);
	}
}

################################################################################

sub
copy_current()
{
	my @new_group;

	clear_current_group;

	# add these things to the main list
	push @things, @new_group;

	# make this group current (we've already cleared current above);
	add_to_current_group(@new_group);

}

################################################################################

sub
delete_current()
{

	# remove the current things
	foreach my $thing (@current_selection) {

		# delete from main list
#		splice @things, $rank, 1;

		# remove the canvas items
#		$canvas->delete($$thing{id}, $$thing{rect_id});

	}

	# don't call clear_current_group, the things no longer exist
	@current_selection = ();

}

################################################################################

sub
togglemenu(@)
{
	my ($parent, $text, $hashref, $command, @labels) = @_;

	my $mb = $parent->Menubutton(
		-text => $text,
		-indicatoron => 1,
		-relief => 'raised',
		-borderwidth => 2,
		-highlightthickness => 2,
		-font => $small_bold_font,
		-anchor => 'c',
		-direction => 'flush',
		);

	my $menu = $mb->Menu(-tearoff => 0);
	$mb->configure(-menu => $menu);

	my $callback = ref($command) =~ /CODE/ ? [$command] : $command;

	foreach (sort @labels) {
		$$hashref{$_} = 1;
		$menu->checkbutton(
			-label => $_,
			-variable => \$$hashref{$_},
			-command => [ @$callback, $_ ],
			);
	}

	$mb;
}

################################################################################

sub 
printargs(@)
{
	$log->debug("args=@_.\n");
}

################################################################################

sub
move_list_item_down(@)
{
	my $lb = shift;

	my $active_index = $lb->index('active');

	return if $active_index == $lb->index('end');

	my $active = $lb->get('active');
	$lb->delete($active_index);
	$lb->insert($active_index+1,$active);

}

################################################################################

sub
move_current(@)
{

	my $debug = 1;

	return unless $#current_selection >= 0;
}

################################################################################

sub
control_switcher (@)
{
	my $var_ref = shift;

	foreach my $control (@_) {
		$control->configure( -state => $$var_ref ? 'normal' : 'disabled');
	}
}

################################################################################

sub
clear_all(@) 
{
	$object_list->delete(0, 'end');
    @things = ();
    @listed_thing_indices = ();
    $new_thing = undef;
    queue_gl_draw();
}

################################################################################

sub write_object(@);
sub
write_object(@)
{
	my ($object, %fopt) = @_;

	$fopt{level} |= 0;

	# don't write the root record
	if ($fopt{level}) {

		$log->debug("$fopt{level}");

		foreach my $property (sort keys %$object) {

			if ($property =~ /parent|first_child|last_child|next_sib|prev_sib|child_paths/) {
				# don't save tree properties
				next;
			}

			$log->debug("$property");
			if ($property =~ /vector/) {
				my $vector_list = $$object{$property};
				print join("", @$vector_list);
			}
			elsif (ref(\$$object{$property}) eq 'SCALAR') {
				# default for simple variables is a simple entry field 
				$log->debug("$$object{$property}");
			}
			else {
				$log->debug("Non-scalar property\n");
			}

		}
		$log->debug("\n");
	}

	$fopt{level}++;

	my $child = $$object{first_child};
	while ($child) {
		write_object($child, %fopt);
		$child = $$child{next_sib};
	}

	$fopt{level}--;

}

################################################################################

sub
save_to_file(@)
{

	my $file = $filename;
    $log->debug("file passed in as $file") if $file;
	$file ||= $main_win->getSaveFile(
		-initialdir => ".", -defaultextension => '.yml', -filetypes => [[ 'Build YML files', [ '.yml' ] ]]);

	return unless $file;

    # we don't want to dump all the temporary _* keys (windows, etc) in each thing
    # Tried YAML::Bless, didn't work first time.
    my @pruned_things = ();
    for my $thing (@things) {
        
        # don't dump series things
        next if $thing->{do_not_save};

        my $pruned_thing = {};
        for my $key ( grep {!/^_/} keys %{ $thing } ) {
            # FWIW, this is a shallow copy
            $pruned_thing->{$key} = $thing->{$key};
        }
        push @pruned_things, $pruned_thing;
    }

    my $save_data = {
        things => \@pruned_things,
        views => \%flat_view,
        gl => $gl_var,
        gl_setting => $gl_setting,
    };

    DumpFile($file, $save_data);

	$log->debug("File $file written ok\n");
    $filename = $file;

}

################################################################################

sub
open_from_file(@)
{

	my $file = shift;
	$file ||= $main_win->getOpenFile(
		-initialdir => ".", -defaultextension => '.yml', -filetypes => [[ 'Build YML files', [ '.yml' ] ]]);

	return unless $file;

    my $load_data = LoadFile($file);
    if (ref $load_data eq 'HASH') {
        @things = @{ $load_data->{things} };

        # restore the view keys we want
        for my $view (keys %flat_view) {
            for my $view_key (qw(x_centre y_centre size render_by_selection render_unselected)) {
                $flat_view{$view}->{$view_key} = $load_data->{views}->{$view}->{$view_key};
            }
        }

        # restore all gl keys
        for my $setting (keys %{ $load_data->{gl} }) {
            for my $setting_key (keys %{ $load_data->{gl}->{$setting} }) {
                $gl_var->{$setting}->{$setting_key} = $load_data->{gl}->{$setting}->{$setting_key};
            }
        }

        # restore all gl settings
        for my $setting (keys %{ $load_data->{gl_setting} }) {
            $gl_setting->{$setting} = $load_data->{gl_setting}->{$setting};
        }

        recalc_views();

    }
    else {
        $log->logdie("no hash from file $file");
    }

    for my $thing (@things) {
        if ($thing->{transform_props}) {
            adjust_transform($thing);
        }
        if ($thing->{series_props}) {
            adjust_series($thing);
        }

        delete $thing->{verts};
    }

    refresh_object_list();

    # HACK
#    map { $_->{listed} = 1; delete $_->{selected}; delete $_->{tk}; } @things;

    update_listed_thing_indices();

    @vertices = ();
    $new_vertex = {};

    queue_gl_draw();
    $filename = $file;
    $log->debug("set filename as $filename");
}

################################################################################

sub
save_pov_file(@)
{

	my $file = shift;
	$file ||= $main_win->getSaveFile(
		-initialdir => "/home/ikm/Documents/pov", -defaultextension => '.pov', -filetypes => [[ 'Pov files', [ '.pov' ] ]]);

	return unless $file;

	unless (open(IN_FH, ">$file")) {
		$log->error( "Couldn't write to file $file: $!\n");
		return;
	}

	print IN_FH $source_text->Contents();

	close IN_FH;

	$log->debug("File written ok\n");

}

################################################################################

sub
open_pov_file(@)
{

	my $file = shift;
	$file ||= $main_win->getOpenFile(
		-initialdir => "/home/ikm/Documents/pov", -defaultextension => '.pov', -filetypes => [[ 'Pov files', [ '.pov' ] ]]);

	return unless $file;

	unless (open(IN_FH, "<$file")) {
		$log->error( "Couldn't read from file $file: $!\n");
		return;
	}

	$source_text->delete('1.0','end');
	my $file_source = "";
	while (<IN_FH>) {
		$file_source .= $_;
	}
	$source_text->insert('end',$file_source);

	close IN_FH;

}

################################################################################

sub
add_child(@)
{
	my ($parent, $child) = @_;

	# existing children?
	if (my $last_child = $$parent{last_child}) {
		# add at end of children
		$$parent{last_child} = $child;
		$$last_child{next_sib} = $child;
		$$child{prev_sib} = $last_child;
		$$child{next_sib} = undef;
	}
	else {
		# must be first child
		$$parent{first_child} = $$parent{last_child} = $child;
		# clear out siblings in case we're pasting
#		$$object{prev_sib} = $$object{next_sib} = undef;
	}

	$$child{parent} = $parent;

}

################################################################################

sub clone_children(@);
sub
clone_children(@)
{
	my ($object, %fopt) = @_;

	$log->debug("clone_children: $object\n");

	# clear the child_paths list for the current object (which should never be 
	# an in-use tree object but a copy of one)
	$log->debug("reset child_paths\n") if $$object{child_paths};
	$$object{child_paths} = {} if $$object{child_paths};

	# recreate all children and update child and sibling links
	if (my $child = $$object{first_child}) {
		while ($child) {
			# create clone
			my $new_child = { %$child };
			add_child($object, $new_child);
			# recurse
			clone_children($new_child);
			# next child
			$child = $$child{next_sib};
		}
	}

	# any reference properties have to be recreated too
	foreach my $key (keys %$object) {
		# listrefs
		if ($key =~ /vector/) {
			$$object{$key} = [ @{$$object{$key}} ];
		}
	}

}

################################################################################

sub copy_object(@);
sub
copy_object(@) 
{
	my ($object, %fopt) = (@_);

	# we will get a null object to copy when creating objects with no
	# individual properties
	$object ||= {};
	# have to re-create all children and links
	my $new_object = { %$object };
	# reset all links for the new object, further additions will set them automatically
	$$new_object{child_paths} = $$new_object{first_child} = $$new_object{last_child} 
		= $$new_object{prev_sib} = $$new_object{next_sib} = undef;

	# recreate all children and update child and sibling links
	if (my $child = $$object{first_child}) {
		while ($child) {
			# create clone
			my $new_child = copy_object($child);
			add_child($new_object, $new_child);
			# next child
			$child = $$child{next_sib};
		}
	}

	# may have a simple list (eg automatic children from create)
	if ($$object{children}) {
		foreach my $child (@{$$object{children}}) {
			my $new_child = copy_object($child);
			add_child($new_object, $new_child);
		}
		# we have to clear the children list after we've used it; the tree links
		# hold the same info and if we cut/copy & paste this item, we'll duplicate
		# the children otherwise.
		delete $$new_object{children};
	}

	# any reference properties have to be recreated too
	foreach my $key (keys %$new_object) {
		# listrefs
		if ($key =~ /vector/) {
			$$new_object{$key} = [ @{$$new_object{$key}} ];
		}
	}

	return $new_object;

}

################################################################################

sub
render_scene(@)
{
	my %fopt = @_;

	$main_win->Busy;
	$source_win->Busy if $source_win;

	my $file = "render.pov";
	open (POV_FH, ">$file") or die "Couldn't open $file: $!";

	if ($fopt{refresh_source}) {
		$current_source = get_povray_source($path_to_thing{''});
		if ($source_text) {
			$source_text->delete('1.0','end');
			$source_text->insert('end',$current_source);
		}
	}
	else {
		$current_source = $source_text->Contents();
	}

	print POV_FH $current_source;
	close POV_FH;
	my $output  = `povray ./$file +P 2>&1`;
	if ($output_text) {
		$output_text->delete('1.0','end');
		$output_text->insert('end', $output);
	}
	$main_win->Unbusy;
	$source_win->Unbusy if $source_win;
}

################################################################################

sub
get_povray_source
{

}

################################################################################

sub
edit_text
{
    my ($thing, $property, $value) = @_;

    my $win = $thing->{_properties_win}->Toplevel(-title => $property);
    $win->transient($thing->{_properties_win});

	my $textbox = $win->Scrolled('TextUndo', -scrollbars => 'osoe',
        %{ $default_widget_type_properties->{Text} },
        -height => 10,
        -width => 50,
		)->grid('-');
    $win->Button(
        -text => 'Refresh', 
        -command => sub {
            $log->debug("save $textbox to $value");
            ${ $value } = $textbox->Contents();
            adjust_series($thing);
        },
        %{ $default_widget_type_properties->{Button}},
    )->grid(
        $win->Button(
            -text => 'Done', 
            -command => sub {
                $log->debug("done");
                $win->destroy();
            },
            %{ $default_widget_type_properties->{Button}},
        ),
    );

    $textbox->Contents(${ $value });
}

################################################################################

sub
display_type_widgets
{
    my ($thing, $type, $type_controls, $type_value, $widget_type_properties) = @_;
    my $log = get_logger();
    $log->debug("$type $type_value");

    for my $row ( @{ $type_controls->{$type_value} } ) {

        my @widgets = ();
        my $widget_name = "";
        for my $widget_spec ( @{ $row } ) {
            if (defined($widget_spec)) {
                my ($name, $widget_type, $spec_props, $default) = @{ $widget_spec };

                # set default if not set yet
                if (!defined($thing->{"${type}_props"}->{$type_value}->{$name})) {
                    $log->debug("set default $name to $default") if $default;
                    $thing->{"${type}_props"}->{$type_value}->{$name} = $default || 0;
                }

                $spec_props ||= {};
                my $widget_specs = { 
                    %{ $default_widget_type_properties->{$widget_type} },
                    %{ $widget_type_properties->{$widget_type} },
                    %{ $spec_props },
                };
                if ($widget_type =~ /button/) {
                    $widget_specs->{-variable} = \$thing->{"${type}_props"}->{$type_value}->{$name};
                }
                elsif ($widget_type =~ /Button/) {
                    # the command sub is already set; add default args
                    my $command = $widget_specs->{-command};
                    $widget_specs->{-command} = [ $command, $thing, $name, \$thing->{"${type}_props"}->{$type_value}->{$name} ];
                }
                else {
                    $widget_specs->{-textvariable} = \$thing->{"${type}_props"}->{$type_value}->{$name};
                }
                if ($name ne $widget_name) {
                    push @widgets, $thing->{"_${type}_frame"}->Label(-text => $name, -font => $small_bold_font);
                    $widget_name = $name;
                }
                push @widgets, $thing->{"_${type}_frame"}->$widget_type(%{ $widget_specs });
            }
            else {
#                push @widgets, '-', '-';
            }
        }
        push @{ $thing->{"_${type}_widgets"} }, @widgets;
        my $first_widget = shift @widgets;
        $first_widget->grid(@widgets, -sticky => 'w');
    }

    $thing->{"${type}_type"} = $type_value;
}

################################################################################

sub
refresh_object_list
{
    my @selected_things = @_;
    my %selected_thing;
    map { $selected_thing{$_} = 1; } @selected_things;

    $object_list->delete(0,'end');
    my $ol_index = 0;
    for my $thing (@things) {
        if ($thing->{listed}) {
            $object_list->insert('end', $thing->{name});
            if ($selected_thing{$thing}) {
                $object_list->selectionSet($ol_index++);
            }
        }
    }

}

################################################################################
        
sub
radiate_vertex_in_plane
{
    my ($vertex, $plane, $angle, $length) = @_;

    $vertex->[$flat_view{$plane}->{x_dim}] += sin(deg2rad($angle)) * $length;
    $vertex->[$flat_view{$plane}->{y_dim}] += cos(deg2rad($angle)) * $length;

}

################################################################################
        
sub
find_ring_points 
{
    my ($anchor_vertex, $plane, $angle, $radius, $angle_change, $count) = @_;

    my @points = ();

    # note that the anchor vertex will not appear in the output list (unless the ring
    # goes right around and generates it again)
    my @pos = @{ $anchor_vertex };

    $angle += $angle_change;
    $angle %= 360;
    for my $index (1 .. $count) {
        radiate_vertex_in_plane(\@pos, $plane, $angle, $radius);
        $angle += $angle_change;
        $angle %= 360;
        push @points, [ @pos ];
    }
    return @points;
}

################################################################################

sub
copy_thing_to_pos
{
    my ($thing, $vertex) = @_;

    my $new_thing = {
        name => $thing->{name},
        type => $thing->{type},
        color => dclone $thing->{color},
        solid => $thing->{solid},
        listed => 0,
        sizes => dclone $thing->{sizes},
        transform_type => $thing->{transform_type},
        # deliberately not dcloning here, so changes to the master's transform
        # will affect the others
        transform_props => $thing->{transform_props},
        _transform_props => $thing->{_transform_props},
        vert => $vertex,
        do_not_save => 1,
    };

    return $new_thing;
}

################################################################################

sub
adjust_series
{
    my $thing = shift @_;
    my $log = get_logger();

    $log->debug("adjust_series $thing");

    # we'll replace any existing series objects for this thing (don't worry about the object listbox,
    # we'll refresh the whole thing)
    $thing->{_series_things} ||= [];
    my $existing_series_size = scalar @{ $thing->{_series_things} };

    # series things follow their owner so we just need to find the owner in the list
    my $index = (firstidx { $_ == $thing } @things) + 1;

    my $props = $thing->{series_props}->{$thing->{series_type}};

    # build the new things
    my @new_things = ();
    my @pos = @{ $thing->{vert} };
    if ($thing->{series_type} eq 'line') {
        for my $index (1 .. $props->{Entries}) {
            $pos[0] += $props->{XInc};
            $pos[1] += $props->{YInc};
            $pos[2] += $props->{ZInc};
            my $new_thing = copy_thing_to_pos($thing, [ @pos ]);
            push @new_things, $new_thing;
        }
    }
    elsif ($thing->{series_type} eq 'arc') {
        my @arc_positions = find_ring_points(
            $thing->{vert},
            $props->{Plane},
            $props->{AngleStart},
            $props->{SideLength},
            $props->{AngleChange},
            $props->{Entries},
            );
        for my $vertex (@arc_positions) {
            my $new_thing = copy_thing_to_pos($thing, $vertex);
            push @new_things, $new_thing;
        }
    }
    elsif ($thing->{series_type} eq 'cloud') {
        my @conditions = ();

        my ($x_min,$x_max) = $props->{XCentred}
            ? (-$props->{XEntries}/2, $props->{XEntries}/2)
            : (0, $props->{XEntries}-1);
        my ($y_min,$y_max) = $props->{YCentred}
            ? (-$props->{YEntries}/2, $props->{YEntries}/2)
            : (0, $props->{YEntries}-1);
        my ($z_min,$z_max) = $props->{ZCentred}
            ? (-$props->{ZEntries}/2, $props->{ZEntries}/2)
            : (0, $props->{ZEntries}-1);

        for my $condition (split(/\n/ms, $props->{Conditions})) {
            my $flags = '';
            if ($condition =~ /(.+):(.+)/) {
                ($condition,$flags) = ($1,$2); 
            }
            $condition =~ s/x/(\$x_index)/g;
            $condition =~ s/y/(\$y_index)/g;
            $condition =~ s/z/(\$z_index)/g;
            $condition = "\$flag = ($condition)";
            $log->debug("condition: $condition");
            push @conditions, $condition;
        }
        for my $x_index ($x_min .. $x_max) {
            for my $y_index ($y_min .. $y_max) {
                for my $z_index ($z_min .. $z_max) {
                    my $flag;
                    for my $condition (@conditions) {
                        eval $condition;
                        last unless $flag;
                    }
                    next unless $flag;
                    $log->debug("$pos[0] $x_index $pos[1] $y_index $pos[2] $z_index");
                    my $vertex = [
                        $pos[0] + $x_index * $props->{XInc},
                        $pos[1] + $y_index * $props->{YInc},
                        $pos[2] + $z_index * $props->{ZInc},
                    ];
                    my $new_thing = copy_thing_to_pos($thing, $vertex);
                    delete $new_thing->{no_gl_render};
                    push @new_things, $new_thing;
                }
            }
        }

        $thing->{no_gl_render} = $props->{HideOriginal};
    }

    # add the things and redraw
    splice @things, $index, $existing_series_size, @new_things;
    $thing->{_series_things} = \@new_things;
    queue_gl_draw();
    refresh_object_list();
    update_listed_thing_indices();

    return;
}

################################################################################

sub
show_series_properties
{
	my ($thing, $series_type) = @_;
    
    $log->debug("show_series_properties $thing $series_type");
    if ($thing->{_series_widgets}) {
        if ( my $first_widget = shift @{ $thing->{_series_widgets} }) {
            $first_widget->gridForget( @{ $thing->{_series_widgets} } );
        }
    }

    # specify controls required to define series of each type
    my $series_type_controls = {
        none => [ ],
        line => [
            [
                [ 'Entries', 'NumEntry', { -minvalue => 1, increment => 1, }, 10 ], 
            ],
            [
                [ 'XInc', 'NumEntry' ], 
                [ 'YInc', 'NumEntry' ], 
                [ 'ZInc', 'NumEntry' ], 
            ],
        ],
        arc => [
            [
                [ 'Entries', 'NumEntry', { -minvalue => 1, increment => 1, }, 10 ], 
            ],
            [
                [ 'AngleStart', 'NumEntry', { -minvalue => 0, -maxvalue => 360, -increment => 5, }, 0 ],
                [ 'AngleChange', 'NumEntry', { -minvalue => -180, -maxvalue => 180, -increment => 5, }, 30 ],
            ],
            [
                [ 'SideLength', 'NumEntry', { -minvalue => 0.5, -increment => 0.5, }, 4 ], 
                [ 'Centred', 'Checkbutton', {} ], 
            ],
            [
                [ 'Plane', 'Radiobutton', { -text => 'XY', -value => 'XY', }, 'XZ' ], 
                [ 'Plane', 'Radiobutton', { -text => 'XZ', -value => 'XZ', } ], 
                [ 'Plane', 'Radiobutton', { -text => 'YZ', -value => 'YZ', } ], 
            ],
        ],
        surface => [ ],
        cloud => [
            [
                [ 'XEntries', 'NumEntry', { -minvalue => 1, increment => 1, } ], 
                [ 'YEntries', 'NumEntry', { -minvalue => 1, increment => 1, } ], 
                [ 'ZEntries', 'NumEntry', { -minvalue => 1, increment => 1, } ], 
            ],
            [
                [ 'XInc', 'NumEntry', {}, 1 ], 
                [ 'YInc', 'NumEntry', {}, 1 ], 
                [ 'ZInc', 'NumEntry', {}, 1 ], 
            ],
            [
                [ 'Conditions', 'Button', { -command => \&edit_text, }, "", ],
            ],
            [
                [ 'XCentred', 'Checkbutton', {}, 1 ], 
                [ 'YCentred', 'Checkbutton', {}, 0 ], 
                [ 'ZCentred', 'Checkbutton', {}, 1 ], 
            ],
            [
                [ 'HideOriginal', 'Checkbutton', {}, 1 ], 
            ],
        ],
    };

    # widget properties by type for this screen
    my $series_widget_type_properties = {
        NumEntry => {
            -browsecmd => [ \&adjust_series, $thing ],
            -command => [ \&adjust_series, $thing ],
            -increment => 1,
        },
        Checkbutton => {
            -command => [ \&adjust_series, $thing ],
        },
        Radiobutton => {
            -command => [ \&adjust_series, $thing ],
        },
        Button => {
            -text => '...',
        },
    };

    display_type_widgets($thing, 'series', $series_type_controls, $series_type, $series_widget_type_properties);

    # display according to current settings
    adjust_series($thing);

    # fixup when reselecting
    $thing->{_properties_win}->update;
    my $width = $thing->{_properties_nb}->width;
    my $height = $thing->{_properties_nb}->height;
    $log->debug("$width $height");
	$thing->{_properties_win}->geometry("${width}x${height}") if $width > 1;

    return;
}

################################################################################

sub
draw_prism
{
    my ($thing, $flag) = @_;
    $flag ||= 0;
    my $log = get_logger();
    my $props = $thing->{transform_props}->{$thing->{transform_type}};
    my $calc_props = $thing->{_transform_props}->{$thing->{transform_type}};
    $log->debug("no props? " . Dumper($thing)) unless $props;
    $log->debug("no calc props? " . Dumper($thing)) unless $calc_props;
#    $log->debug("draw_prism $flag");

    # all the calculation is done in adjust_transform; here we just spit out the primitives

    # draw prism ends
    for my $end (0,1) {

        # done as triangle fans
        glBegin(GL_TRIANGLE_FAN);

        # normal away from end
        glNormal3f(@{ $calc_props->{end_normal}->[$end] });

        # list of vertices; centre of end, first rim point, then around the rim and repeat the first point
        for my $vertex (@{ $calc_props->{end_vertices}->[$end] }) {
            glVertex3f( @{ $vertex } );
        }

        # end end
        glEnd();
    }

    # draw sides
    glBegin(GL_QUAD_STRIP);
    # first two points start the first side, remaining pairs each complete a side and are preceded by a normal
    glNormal3f(@{ $calc_props->{side_normals}->[$#{ $calc_props->{side_normals} } ] });
    glVertex3f( @{ $calc_props->{side_points}->[0] } );
    glVertex3f( @{ $calc_props->{side_points}->[1] } );
    for my $index ( 0 .. $#{ $calc_props->{side_normals} } ) {
        glNormal3f(@{ $calc_props->{side_normals}->[$index] });
        glVertex3f( @{ $calc_props->{side_points}->[($index + 1) * 2] } );
        glVertex3f( @{ $calc_props->{side_points}->[($index + 1) * 2 + 1] } );
    }
    glEnd();
    
    return;
}

################################################################################

sub
draw_ramp
{
    my ($thing, $flag) = @_;
    $flag ||= 0;
    my $log = get_logger();
    my $props = $thing->{transform_props}->{$thing->{transform_type}};
    my $calc_props = $thing->{_transform_props}->{$thing->{transform_type}};
    $log->debug("no props? " . Dumper($thing)) unless $props;

    # all the calculation is done in adjust_transform; here we just spit out the primitives

    # draw ramp ends
    for my $end (0,1) {

        # done as a single triangle
        glBegin(GL_TRIANGLES);

        # normal away from end
        glNormal3f(@{ $calc_props->{end_normal}->[$end] });

        # list of vertices; just the three points that define the triangle
        for my $vertex (@{ $calc_props->{end_vertices}->[$end] }) {
            glVertex3f( @{ $vertex } );
        }

        # end end
        glEnd();
    }

    # draw sides
    glBegin(GL_QUAD_STRIP);
    # first two points start the first side, remaining pairs each complete a side and are preceded by a normal
    glNormal3f(@{ $calc_props->{side_normals}->[$#{ $calc_props->{side_normals} } ] });
    glVertex3f( @{ $calc_props->{side_points}->[0] } );
    glVertex3f( @{ $calc_props->{side_points}->[1] } );
    for my $index ( 0 .. $#{ $calc_props->{side_normals} } ) {
        glNormal3f(@{ $calc_props->{side_normals}->[$index] });
        glVertex3f( @{ $calc_props->{side_points}->[($index + 1) * 2] } );
        glVertex3f( @{ $calc_props->{side_points}->[($index + 1) * 2 + 1] } );
    }
    glEnd();
    
    return;
}

################################################################################

sub
adjust_transform
{
    my $thing = shift @_;
    my $log = get_logger();

    $log->debug("adjust_transform $thing $thing->{transform_type}");
    my $props = $thing->{transform_props}->{$thing->{transform_type}};
    my $calc_props = $thing->{_transform_props}->{$thing->{transform_type}} ||= {};

    my @side_points;

    if ($thing->{transform_type} eq 'none') {
        $thing->{type} = $OT_CUBOID;
    }
    elsif ($thing->{transform_type} eq 'prism') {
        # properties:
        # Sides, Plane, Angle, Size, CustomSize, 

        # we need to define the normals and vertices used by draw_prism.
        # Note that the object should be positioned centred around the origin;
        # it will be translated into the correct location on rendering.

        my $radius;
        if ($props->{Size} eq 'Small') {
            $radius = min($thing->{sizes}->[$flat_view{$props->{Plane}}->{x_dim}], $thing->{sizes}->[$flat_view{$props->{Plane}}->{y_dim}]);
            $radius /= 2;
        }
        elsif ($props->{Size} eq 'Large') {
            $radius = max($thing->{sizes}->[$flat_view{$props->{Plane}}->{x_dim}], $thing->{sizes}->[$flat_view{$props->{Plane}}->{y_dim}]);
            $radius /= 2;
        }
        elsif ($props->{Size} eq 'Custom') {
            $radius = $props->{CustomSize};
        }
        else {
            $log->logdie("bad size $props->{Size}");
        }

        my $angle_change = 360 / $props->{Sides};

        my $start_angle = 90 - ($angle_change / 2) + $props->{Angle};

        # define the ends
        delete $calc_props->{end_vertices};
        for my $end (0,1) {

            # the centre of the prism is always at 0,0 wrt the plane of the prism
            my $centre_end = [ 0,0,0 ];

            # move to positive or negative in the dimension of the prism
            $centre_end->[$flat_view{$props->{Plane}}->{other_dim}] += ($end ? 1 : -1) * ($thing->{sizes}->[$flat_view{$props->{Plane}}->{other_dim}] / 2);

            # the centre point is the first point we need for rendering
            push @{ $calc_props->{end_vertices}->[$end] }, $centre_end;

            my $end_radius = $radius * ($end ? $props->{PositiveEndSize} : $props->{NegativeEndSize});

            # move centre to first point on rim
            my $first_point = dclone $centre_end;
            radiate_vertex_in_plane($first_point, $props->{Plane}, $props->{Angle}, $end_radius);

            my $side_length = 2 * $end_radius * sin(deg2rad($angle_change/2));

            # generate points around rim; note that the first point only appears in this list as the final point,
            # since we define the angle change and count to do a full revolution.
            my @rim_points = find_ring_points(
                $first_point,
                $props->{Plane},
                $start_angle,
                $side_length,
                $angle_change,
                $props->{Sides},
                );

            # the original point on the rim is the next rendering point
            unshift @rim_points, $first_point;

            # we now have the complete list of points around the rim; by combining these lists from
            # both ends we get the quad_strip lists for the side faces.
            if (@side_points) {
                # second loop; mesh them together
                $calc_props->{side_points} = [ mesh @rim_points, @side_points ];
            }
            else {
                # first loop, just record them
                @side_points = @rim_points;
            }

            # add the rim points
            push @{ $calc_props->{end_vertices}->[$end] }, @rim_points;

            # we can now use the first 3 points in the vertices list to find the normal to the plane
            my $end_flag = $props->{Plane} eq 'XY' ? ! $end : $end;
            if ($end_flag) {
                $calc_props->{end_normal}->[$end] = [ triangle_normal(
                    @{ $calc_props->{end_vertices}->[$end]->[0] },
                    @{ $calc_props->{end_vertices}->[$end]->[1] },
                    @{ $calc_props->{end_vertices}->[$end]->[2] },
                    ) ];
            }
            else {
                $calc_props->{end_normal}->[$end] = [ triangle_normal(
                    @{ $calc_props->{end_vertices}->[$end]->[0] },
                    @{ $calc_props->{end_vertices}->[$end]->[2] },
                    @{ $calc_props->{end_vertices}->[$end]->[1] },
                    ) ];
            }

        }

        # calculate side normals
        $calc_props->{side_normals} = [];
        for my $index (1 .. scalar @{ $calc_props->{side_points}} / 2 - 1) {
            if ($props->{Plane} eq 'XY') {
                push @{ $calc_props->{side_normals} },
                    [ triangle_normal(
                        @{ $calc_props->{side_points}->[ $index * 2 - 2] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 0] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 1] },
                    )];
            }
            else {
                push @{ $calc_props->{side_normals} },
                    [ triangle_normal(
                        @{ $calc_props->{side_points}->[ $index * 2 - 2] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 1] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 0] },
                    )];
            }
        }

        $log->debug("transform props: " . Dumper($props));
        $log->debug("transform calc props: " . Dumper($calc_props));
        $thing->{type} = $OT_PRISM;

    }
    elsif ($thing->{transform_type} eq 'stairs') {
        # props: Steps, Plane, Rotation

        my $plane_view = $flat_view{$props->{Plane}};

        # define the ends
        delete $calc_props->{end_vertices};
        for my $end (0,1) {

            # anticlockwise for the -ve end
            my @corners = (
                [ -1,1 ],
                [ 1,1 ],
                [ 1,-1 ],
                [ -1,-1 ],
            );

            # we rotate the corners for the end then use the first 3 as the corners for the end triangle
            for my $i (1 .. $props->{Rotation}) {
                push @corners, shift @corners;
            }

            $log->debug("corners = " . Dumper(\@corners));

            for my $i (0 .. 2) {
                my $vertex = [];
                $vertex->[$plane_view->{other_dim}] = ($end ? 1 : -1) * ($thing->{sizes}->[$plane_view->{other_dim}] / 2);
                $vertex->[$plane_view->{x_dim}] = $corners[$i]->[0] * ($thing->{sizes}->[$plane_view->{x_dim}] / 2);
                $vertex->[$plane_view->{y_dim}] = $corners[$i]->[1] * ($thing->{sizes}->[$plane_view->{y_dim}] / 2);
                push @{ $calc_props->{end_vertices}->[$end] }, $vertex;
            }

            # we can now use the first 3 points in the vertices list to find the normal to the end plane
            my $end_flag = $props->{Plane} eq 'XY' ? ! $end : $end;
            if ($end_flag) {
                $calc_props->{end_normal}->[$end] = [ triangle_normal(
                    @{ $calc_props->{end_vertices}->[$end]->[0] },
                    @{ $calc_props->{end_vertices}->[$end]->[1] },
                    @{ $calc_props->{end_vertices}->[$end]->[2] },
                    ) ];
            }
            else {
                $calc_props->{end_normal}->[$end] = [ triangle_normal(
                    @{ $calc_props->{end_vertices}->[$end]->[0] },
                    @{ $calc_props->{end_vertices}->[$end]->[2] },
                    @{ $calc_props->{end_vertices}->[$end]->[1] },
                    ) ];
            }

            if ($props->{Steps} == 1) {

                # Steps == 1 is a ramp; the 3 points is all we need for the ends
                $thing->{type} = $OT_RAMP;

            }
            else {
                $thing->{type} = $OT_STAIRS;

                # Steps > 1 means we need to add steps between the diagonally opposed points,
                # which are the first and last in the end triangle (since we start with a list of
                # 4 points going around the square and then leave one off the end to find the triangle).
                my $from = $calc_props->{end_vertices}->[$end]->[2];
                my $to = $calc_props->{end_vertices}->[$end]->[0];
                my $x_inc = ($to->[$plane_view->{x_dim}] - $from->[$plane_view->{x_dim}]) / $props->{Steps};
                my $y_inc = ($to->[$plane_view->{y_dim}] - $from->[$plane_view->{y_dim}]) / $props->{Steps};
                for my $i (1 .. $props->{Steps}) {

                    # each step is a triangle joined onto the ramp triangle

                    my $vertex = dclone $from;
                    $vertex->[$plane_view->{x_dim}] += $x_inc * ($i - 1);
                    $vertex->[$plane_view->{y_dim}] += $y_inc * ($i - 1);
                    push @{ $calc_props->{end_vertices}->[$end] }, $vertex;

                    $vertex = dclone $from;
                    if ($props->{Rotation} % 2) {
                        $vertex->[$plane_view->{x_dim}] += $x_inc * ($i - 1);
                        $vertex->[$plane_view->{y_dim}] += $y_inc * $i;
                    }
                    else {
                        $vertex->[$plane_view->{x_dim}] += $x_inc * $i;
                        $vertex->[$plane_view->{y_dim}] += $y_inc * ($i - 1);
                    }
                    push @{ $calc_props->{end_vertices}->[$end] }, $vertex;

                    $vertex = dclone $from;
                    $vertex->[$plane_view->{x_dim}] += $x_inc * $i;
                    $vertex->[$plane_view->{y_dim}] += $y_inc * $i;
                    push @{ $calc_props->{end_vertices}->[$end] }, $vertex;

                }
            }

            # For the purpose of drawing the sides, the rim points have the start point at the end as well.
            # To do the mesh thing, we have to have a real array variable, we can't use a () construct
            my @rim_points = ( @{ $calc_props->{end_vertices}->[$end] },
                $calc_props->{end_vertices}->[$end]->[0]);

            # now we have the full end point lists, we can transform these lists into the side lists
            if (@side_points) {
                # second loop; mesh them together
                $calc_props->{side_points} = [ mesh @rim_points, @side_points ];
            }
            else {
                # first loop, just record them
                @side_points = @rim_points;
            }

        }

        # calculate side normals
        $calc_props->{side_normals} = [];
        for my $index (1 .. scalar @{ $calc_props->{side_points}} / 2 - 1) {
            if ($props->{Plane} eq 'XY') {
                push @{ $calc_props->{side_normals} },
                    [ triangle_normal(
                        @{ $calc_props->{side_points}->[ $index * 2 - 2] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 0] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 1] },
                    )];
            }
            else {
                push @{ $calc_props->{side_normals} },
                    [ triangle_normal(
                        @{ $calc_props->{side_points}->[ $index * 2 - 2] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 1] },
                        @{ $calc_props->{side_points}->[ $index * 2 - 0] },
                    )];
            }
        }

        $log->debug("transform props: " . Dumper($props));
        $log->debug("transform calc props: " . Dumper($calc_props));
    }

    queue_gl_draw();
    return;
}

################################################################################

sub
show_transform_properties
{
	my ($thing, $transform_type) = @_;

    $log->debug("show_transform_properties $thing $transform_type");
    if ($thing->{_transform_widgets}) {
        if ( my $first_widget = shift @{ $thing->{_transform_widgets} }) {
            $first_widget->gridForget( @{ $thing->{_transform_widgets} } );
        }
    }

    # specify controls required to define transform of each type
    my $transform_type_controls = {
        none => [ ],
        prism => [
            [
                [ 'Sides', 'NumEntry', { -minvalue => 3, increment => 1, }, 5 ], 
            ],
            [
                [ 'Plane', 'Radiobutton', { -text => 'XY', -value => 'XY', }, 'XZ' ], 
                [ 'Plane', 'Radiobutton', { -text => 'XZ', -value => 'XZ', } ], 
                [ 'Plane', 'Radiobutton', { -text => 'YZ', -value => 'YZ', } ], 
            ],
            [
                [ 'Angle', 'NumEntry', { -minvalue => 0, -maxvalue => 360, }, 0 ], 
            ],
            [
                [ 'Size', 'Radiobutton', { -text => 'Small', -value => 'Small', }, 'Small' ], 
                [ 'Size', 'Radiobutton', { -text => 'Large', -value => 'Large', } ], 
                [ 'Size', 'Radiobutton', { -text => 'Custom', -value => 'Custom', } ], 
            ],
            [
                [ 'CustomSize', 'NumEntry', { -minvalue => 0.1, increment => 0.5, }, 2 ], 
            ],
            [
                [ 'NegativeEndSize', 'NumEntry', { -minvalue => 0, -increment => 0.1, }, 1 ], 
                [ 'PositiveEndSize', 'NumEntry', { -minvalue => 0, -increment => 0.1, }, 1 ], 
            ],
        ],
        stairs => [
            [
                [ 'Steps', 'NumEntry', { -minvalue => 1, increment => 1, }, 1 ], 
            ],
            [
                [ 'Plane', 'Radiobutton', { -text => 'XY', -value => 'XY', }, 'XY' ], 
                [ 'Plane', 'Radiobutton', { -text => 'XZ', -value => 'XZ', } ], 
                [ 'Plane', 'Radiobutton', { -text => 'YZ', -value => 'YZ', } ], 
            ],
            [
                [ 'Rotation', 'NumEntry', { -minvalue => 1, -maxvalue => 4, increment => 1, }, 1 ], 
            ],
        ],
        point => [ ],
        arch => [ ],
    };

    # widget properties by type for this screen
    my $transform_widget_type_properties = {
        NumEntry => {
            -browsecmd => [ \&adjust_transform, $thing ],
            -command => [ \&adjust_transform, $thing ],
            -increment => 1,
        },
        Checkbutton => {
            -command => [ \&adjust_transform, $thing ],
        },
        Radiobutton => {
            -command => [ \&adjust_transform, $thing ],
        },
    };

    display_type_widgets($thing, 'transform', $transform_type_controls, $transform_type, $transform_widget_type_properties);

    # display according to current settings
    adjust_transform($thing);

    # fixup when reselecting
    $thing->{_properties_win}->update;
    my $width = $thing->{_properties_nb}->width;
    my $height = $thing->{_properties_nb}->height;
    $log->debug("$width $height");
	$thing->{_properties_win}->geometry("${width}x${height}") if $width > 1;

    return;
}

################################################################################

sub
show_properties
{
	my $thing = shift;

    my $properties_win;
	if (Exists($thing->{_properties_win})) {
        $properties_win = $thing->{_properties_win};
		$properties_win->deiconify();
		$properties_win->raise();
		$thing->{_properties_tab}->gridForget(@{ $thing->{_property_widgets} });
	}
	else {
		$thing->{_properties_win} = $properties_win = $main_win->Toplevel();
		$properties_win->transient($main_win);
		$properties_win->protocol('WM_DELETE_WINDOW', sub { $properties_win->withdraw });
        $properties_win->bind("<KeyPress>", [ \&key_handler, Ev('K') ] );

        my $properties_nb = $thing->{_properties_nb} = $properties_win->NoteBook()->grid('-','-','-', -sticky => 'nsew');
        $thing->{_properties_tab} = $properties_nb->add('properties', -label => 'Properties');

        # transform properties
        $thing->{_transform_tab} = $properties_nb->add('transform', -label => 'Transform');
        $thing->{transform_type} ||= 'none';
        $thing->{_transform_tab}->Label(-text => 'Type', -font => $small_bold_font)->grid(
            $thing->{_transform_tab}->Optionmenu(  
                -variable => \$thing->{transform_type},
                -options => [ qw(none prism stairs point arch) ],
                -command => [ \&show_transform_properties, $thing ],
            ),
        );
        $thing->{_transform_frame} = $thing->{_transform_tab}->Frame()->grid('-');

        # series properties
        $thing->{_series_tab} = $properties_nb->add('series', -label => 'Series');
        $thing->{series_type} ||= 'none';
        $thing->{_series_tab}->Label(-text => 'Type', -font => $small_bold_font)->grid(
            $thing->{_series_tab}->Optionmenu(  
                -variable => \$thing->{series_type},
                -options => [ qw(none line arc surface cloud) ],
                -command => [ \&show_series_properties, $thing ],
            ),
        );
        $thing->{_series_frame} = $thing->{_series_tab}->Frame()->grid('-');
	}

#    $log->debug("properties for $thing->{name}: " . Dumper($thing));

    $properties_win->title("Properties for $thing->{name}");
    $thing->{_property_widgets} = [];

	foreach my $property (sort keys %$thing) {
        next if $property =~ /^_/;
		my @widgets = ();
		my @spans = ();

		if (ref(\$thing->{$property}) eq 'SCALAR') {
			# default for simple variables is a simple entry field 
			push @widgets, $thing->{_properties_tab}->Entry(
				-font => $small_font,
				-textvariable => \$thing->{$property},
                -background => 'white',
				-foreground => 'black',
				);
			@spans = (qw/- - - - -/);
		}
        elsif (ref $thing->{$property} eq 'ARRAY') {
            $log->debug("display array for $property");
            push @widgets, my $frame = $thing->{_properties_tab}->Frame();
            my @subwidgets = ();
            for my $index (0 .. $#{ $thing->{$property} }) {
                push @subwidgets, $frame->Entry(
                    -font => $small_font,
                    -textvariable => \$thing->{$property}->[$index],
                    -width => 5,
                    -background => 'white',
                );
            }
            my $first_widget = shift @subwidgets;
            $first_widget->grid(@subwidgets) if $first_widget;
        }
        elsif (ref $thing->{$property} eq 'HASH') {
            $log->debug("display hash for $property");
            push @widgets, my $frame = $thing->{_properties_tab}->Frame();
            my @subwidgets = ();
            for my $key (sort keys %{ $thing->{$property} }) {
                push @subwidgets, $frame->Label(
                    -font => $small_bold_font,
                    -text => $key,
                    );
                push @subwidgets, $frame->Entry(
                    -width => 5,
                    -background => 'white',
                    -font => $small_font,
                    -textvariable => \$thing->{$property}->{$key},
                );
            }
            my $first_widget = shift @subwidgets;
            $first_widget->grid(@subwidgets) if $first_widget;
        }
		else {
			# everything else gets a label
			push @widgets, $thing->{_properties_tab}->Label(-text => sprintf("%s",$thing->{$property}));
			@spans = (qw/- - - - -/);
		}

		push @{ $thing->{_property_widgets} },
            $thing->{_properties_tab}->Button(
                    -text => "$property",  
                    -relief => 'groove', 
                    -command => sub { queue_gl_draw(); },
                    -width => 15,
                    -pady => 0,
                    -font => $small_bold_font,
            )->grid(@widgets, @spans, -sticky => 'w');
		push @{ $thing->{_property_widgets} }, @widgets;


	}

    $thing->{_properties_win}->update;
    show_series_properties($thing, $thing->{series_type});
    show_transform_properties($thing, $thing->{transform_type});
#	push @property_widgets, $properties_win->Button(
#		-text => "Render", -command => sub { render_scene(refresh_source => 1); }
#			)->grid(-columnspan => 7, -sticky => 'ew');

}

################################################################################

sub
draw_line(@) 
{

	if ((scalar keys %line_points) > 1) {
		$line_canvas->delete($line_id) if $line_id;
		$line_id = $line_canvas->createLine(@line_coords, -smooth => $smooth_line, -splinesteps => $splinesteps);
		$line_canvas->lower($line_id);
	}

	return 1;
}

################################################################################

sub
move_line_point(@) 
{

	return unless $moving_line_point;

	$log->debug("move_line_point @_\n") if $debug;

	my ($canvas, $x, $y) = @_;

	$canvas->coords($moving_line_point,$x-5,$y-5,$x+5,$y+5);
	$log->debug("moving_line_point, $line_points{$moving_line_point}\n");
	splice @line_coords, $line_points{$moving_line_point}, 2, ($x, $y);

	draw_line;
}

################################################################################

sub
line_click(@) 
{

	$log->debug("line_click: @_\n");

	my ($canvas, $x, $y, $press) = @_;
	my $refresh_line = 0;
	my $release = ! $press;

	my $current = $canvas->find(withtag => "current");
	$current = shift @$current if $current;
	# ignore clicks on lines
	$current = undef unless grep /point/, $canvas->gettags($current);

	if ($add_on_line_click && $release) {
		if ($current) {
			$log->debug("over another point, no add\n");
		}
		else {
			my $new_point = $canvas->createOval($x-5,$y-5,$x+5,$y+5,-fill=>'red',-outline=>'red',-tags => [ 'point' ] );
			push @line_coords, $x,$y;
			$line_points{$new_point} = $#line_coords - 1;
			$log->debug("added $new_point, lp = $line_points{$new_point}\n");

			$refresh_line = 1;
		}
	}

	if ($press && $current) {
		$log->debug("moving $current\n");
		$moving_line_point = $current;
	}
	else {
		$log->debug("not moving\n");
		$moving_line_point = undef;
	}

	draw_line if $refresh_line;
}

################################################################################

sub 
drag_tracker {
# click-movement sub
	my ($canvas, $button_still_down, $x, $y) = @_;

	$log->debug("+drag_tracker+ at $x,$y\n") if $debug;
    
    my $view = $canvas_view{$canvas};

    $view->{dragging} = 1;

    # record the position as the snap position so the click handler gets the right point
    $view->{snap_x} = $x;
    $view->{snap_y} = $y;

    if ($drag_to_show_flag) {

        # adjust the show filter coordinates controlled by this view
        my ($min_x, $max_x) = sort numeric ($x, $view->{prev_drag_x});
        my ($min_y, $max_y) = sort rev_numeric ($y, $view->{prev_drag_y});

        my @coord = view_xy_to_model_xyz($view, $min_x, $min_y);
        for my $dim (0,1,2) {
            $show_filter_pos_min[$dim] = $coord[$dim] if defined $coord[$dim];
        }
        @coord = view_xy_to_model_xyz($view, $max_x, $max_y);
        for my $dim (0,1,2) {
            $show_filter_pos_max[$dim] = $coord[$dim] if defined $coord[$dim];
        }
        $filter_shown_by_position = 1;
    }
    else {

        # adjust the views according to the displacement from the last drag point
        my $delta_x = $x - $view->{prev_drag_x};
        my $delta_y = $y - $view->{prev_drag_y};
        $log->debug("drag delta = $delta_x, $delta_y");

        # convert to model coords; this is not a true transformation of canvas coords (since all
        # we have are offsets) so we can't use view_xy_to_model_xyz().
        $delta_x = $delta_x / $view->{size_factor};
        $delta_y = $delta_y / $view->{size_factor};

        $view->{x_centre} += $delta_x;
        $view->{y_centre} += $delta_y;

        ($view->{prev_drag_x}, $view->{prev_drag_y}) = ($x,$y);

    }
    recalc_views();
    refresh_views();

    # the refresh deletes all indicators so do this now
    if ($drag_to_show_flag) {

        # we never update prev_drag_* so we can draw the rectangle to there every time
        $view->{canvas}->coords($view->{new_object_indicator}, $view->{prev_drag_x},$view->{prev_drag_y}, $x,$y);
    }

}

################################################################################

sub 
move_tracker {
# click-movement sub
	my ($canvas, $x, $y, $flags) = @_;
#    $log->debug("move_tracker: $canvas, $x, $y");

    my $view = $current_view = $canvas_view{$canvas};

    # ignore early clicks before size factor calced
    return unless $view->{size_factor};

    my @model_coords = view_xy_to_model_xyz($view, $x, $y);

    my $do_gl_draw = 0;

    # only 2 model coords will ever be set in @model_coords, so leave the other unchanged
    $cursor_statusvar[0] = $model_coords[0] || $cursor_statusvar[0];
    $cursor_statusvar[1] = $model_coords[1] || $cursor_statusvar[1];
    $cursor_statusvar[2] = $model_coords[2] || $cursor_statusvar[2];

    if ($gl_setting->{eye_follows_cursor}) {
        $gl_var->{eye}->{x} = $cursor_statusvar[0];
        $gl_var->{eye}->{y} = $cursor_statusvar[1];
        $gl_var->{eye}->{z} = $cursor_statusvar[2];
        $do_gl_draw = 1;
    }

    if ($gl_setting->{light_follows_cursor}) {
        $gl_var->{light}->{x} = $cursor_statusvar[0];
        $gl_var->{light}->{y} = $cursor_statusvar[1];
        $gl_var->{light}->{z} = $cursor_statusvar[2];
        $do_gl_draw = 1;
    }

    if ($gl_setting->{target_follows_cursor}) {
        $gl_var->{target}->{x} = $cursor_statusvar[0];
        $gl_var->{target}->{y} = $cursor_statusvar[1];
        $gl_var->{target}->{z} = $cursor_statusvar[2];
        $do_gl_draw = 1;
    }

    unless ($flags->{no_snap}) {
        # check snaps to see if we should warp the selection lines onto a snap
        for my $snap ( @{ $view->{snaps} } ) {
            if ($x > $snap->[0] - $snap_radius && $x < $snap->[0] + $snap_radius) {
                $x = $snap->[0];
            }
            if ( $y > $snap->[1] - $snap_radius && $y < $snap->[1] + $snap_radius) {
                $y = $snap->[1];
            }
        }
    }

    # record the snapped position; we'll use this instead of the mouse position if a click is received
    $view->{snap_x} = $x;
    $view->{snap_y} = $y;

    # recalc model coords so all views show the snap; last snap wins
    @model_coords = view_xy_to_model_xyz($view, $x, $y);

    # calculate lines for all views from model coords since their scales and offsets may be different
    for my $other_view (values %flat_view) {

        my @view_coords = model_xyz_to_view_xy($other_view, @model_coords);

        # we don't display the selection line if actions in another view have already defined this dimension
        if ( $new_vertex->{from_view} && $new_vertex->{from_view} ne $view->{name} && defined $new_vertex->{vertex}->[$other_view->{x_dim}] ) {
            $other_view->{canvas}->coords($other_view->{selection_line_x}, 0,0,0,0);
        }
        else {
            $other_view->{canvas}->coords($other_view->{selection_line_x}, $view_coords[0], 0, $view_coords[0], $view_height)
        }

        # 
        if ( $new_vertex->{from_view} && $new_vertex->{from_view} ne $view->{name} && defined $new_vertex->{vertex}->[$other_view->{y_dim}] ) {
            $other_view->{canvas}->coords($other_view->{selection_line_y}, 0,0,0,0);
        }
        else {
            $other_view->{canvas}->coords($other_view->{selection_line_y}, 0, $view_coords[1], $view_width, $view_coords[1])
        }

        if ($#vertices > -1) {
            my $previous_vertex = $vertices[$#vertices];
            my @previous_view_coords = model_xyz_to_view_xy($other_view, @{ $previous_vertex->{vertex} } );
            if ($new_vertex->{vertex}) {
                # we have a previous vertex and an in-progress (ie incomplete by 1 dim) vertex
                if ($view->{name} eq $new_vertex->{from_view}) {
                    # if we're on the view that originated the in-progress, we only have 2 dimensions
                    # so we display only on that view
                    if ($other_view->{name} eq $new_vertex->{from_view}) {
                        $other_view->{canvas}->coords($other_view->{new_object_indicator}, @previous_view_coords, @view_coords);
                    }
                    else {
                        $other_view->{canvas}->coords($other_view->{new_object_indicator}, 0,0,0,0);
                    }
                }
                else {
                    # we've changed views, so combination of in-progress vertex and current position defines a 3d-point,
                    # so we have a potential final shape to display on all views.

                    $log->debug("new vertex = " . Dumper($new_vertex));

                    # combine in-progress vertex and current location
                    my @point = @{ $new_vertex->{vertex} };
                    for my $i (0 .. 2) {
                        if (defined($model_coords[$i]) && ! defined($point[$i])) {
                            $point[$i] = $model_coords[$i];
                        }
                    }

                    # display box
                    my @point_coords = model_xyz_to_view_xy($other_view, @point );
                    $other_view->{canvas}->coords($other_view->{new_object_indicator}, @previous_view_coords, @point_coords);

                    # display a wire-frame version in opengl during final point selection
                    unless ($new_thing) {
                        $new_thing = {
                            name => 'new cuboid',
                            type => $OT_CUBOID,
                            color => [ 1, 0, 0 ],
                            solid => 0,
                            listed => 1,
                        };
                        push @things, $new_thing;
                        push @listed_thing_indices, $#things;
                        $object_list->insert('end', $new_thing->{name});
                    }
                    my ($p1,$p2) = (\@point, $previous_vertex->{vertex});
                    for my $i (0 .. 2) {
                        my ($min, $max) = sort { $a <=> $b } ($p1->[$i], $p2->[$i]);
                        $new_thing->{vert}->[$i] = $min;
                        $new_thing->{sizes}->[$i] = $max - $min;
                    }
                    $do_gl_draw = 1;

                }
            }
            else {
                if ($other_view == $view) {
                    $other_view->{canvas}->coords($other_view->{new_object_indicator}, @previous_view_coords, @view_coords);
                }
                else {
                    $other_view->{canvas}->coords($other_view->{new_object_indicator}, 0,0,0,0);
                }
            }
        }
    }

    queue_gl_draw() if $do_gl_draw;

}

################################################################################

sub 
click_tracker {
    # click handler
	my ($canvas, $release, $x, $y) = @_;

    my $view = $canvas_view{$canvas};

    # use the latest snap position instead of the cursor position; no snap
    # means we've clicked before moving, eg an obscuring window has been removed AND
    # we've never moved over the view before. Ignore it to prevent undef warnings.
    return unless defined($view->{snap_x}) && defined($view->{snap_y});

    ($x,$y) = ($view->{snap_x}, $view->{snap_y});

	$log->debug("+click_tracker+ release $release on $view->{name} at $x,$y\n");

    if ($release) {
        $log->debug("release, press at $view->{prev_drag_x}, $view->{prev_drag_y}\n");
        if ($drag_to_show_flag) {
            $view->{canvas}->coords($view->{new_object_indicator}, 0,0,0,0);
            $drag_to_show_flag = 0;
            return;
        }
        else {
            if ($view->{dragging}) {
                # this is the finish of a drag event, ie button-up; no processing required as yet
                ($view->{prev_drag_x}, $view->{prev_drag_y}) = (undef, undef);
                return;
            }
        }
    }
    else {
        # record the first drag start
        ($view->{prev_drag_x}, $view->{prev_drag_y}) = ($x,$y);
        $view->{dragging} = 0;
        return;
    }

    my @model_coords = view_xy_to_model_xyz($view, $x, $y);

    # scenarios (working in XY as an example)
    # click in XY defines x & y
    # further clicks in XY will redefine x & y in current point
    # selection lines displayed as normal in XY
    # next click in YZ or XZ will set z only
    # selection line for non-z dimensions not displayed in YZ & XZ

    # only 2 model coords will ever be set in @model_coords, so leave the other unchanged
    my $complete = 1;
    for my $i (0 .. 2) {
        if (! $new_vertex->{from_view} || $new_vertex->{from_view} eq $view->{name}) {
            # subsequent clicks in the same view just reset the initial 2 dims
            $new_vertex->{vertex}->[$i] = $model_coords[$i];
            $new_vertex->{from_view} = $view->{name};
            $complete = 0;
        }
        else {
            $new_vertex->{vertex}->[$i] = $model_coords[$i] unless defined $new_vertex->{vertex}->[$i];
            $complete = 1;
        }
        $selected_statusvar[$i] = $new_vertex->{vertex}->[$i] || '';
    }

    if ($complete) {

        push @vertices, $new_vertex;
        $log->debug("finish vertex $#vertices " . Dumper($new_vertex));
        if ($#vertices % 2) {
            my ($v1,$v2) = splice @vertices, 0, 2;

            # new_thing is already in list, so just update the location and make it solid
            $new_thing->{solid} = 1;
            for my $i (0 .. 2) {
                my ($min, $max) = sort { $a <=> $b } ($v1->{vertex}->[$i], $v2->{vertex}->[$i]);
                $new_thing->{vert}->[$i] = $min;
                $new_thing->{sizes}->[$i] = $max - $min;
            }
            $object_list->selectionSet('end');
            # force selection event
            object_list_select_handler($object_list);

            # this leaves the new_thing in the list, just clear the working version so we can create new ones
            $new_thing = undef;

            queue_gl_draw();
        }
        $new_vertex = {};

    }
    else {

        $log->debug("start vertex");

    }

    refresh_views();

}

################################################################################

sub
undo_last_click
{
    # undo button; back up a step
    if (scalar @vertices) {
        if ($new_vertex->{from_view}) {
            $log->debug("undo c");
            $new_vertex = {};
            for my $other_view (values %flat_view) {
                $other_view->{canvas}->coords($other_view->{selected_line_x}, 0,0,0,0);
                $other_view->{canvas}->coords($other_view->{selected_line_y}, 0,0,0,0);
            }
            if ($new_thing) {
                $new_thing = undef;
                $object_list->delete('end');
                pop @things;
                pop @listed_thing_indices;
            }
        }
        else {
            $log->debug("undo b");
            # the top vertex on @vertices should become $new_vertex instead, and we should
            # clear the last dimension set, which is the dimension undefined for the view
            # that created the vertex
            $new_vertex = pop @vertices;
            if ($new_vertex->{from_view} eq 'XZ') {
                $new_vertex->{vertex}->[1] = undef;
            }
            elsif ($new_vertex->{from_view} eq 'YZ') {
                $new_vertex->{vertex}->[0] = undef;
            }
            elsif ($new_vertex->{from_view} eq 'XY') {
                $new_vertex->{vertex}->[2] = undef;
            }
            else {
                die;
            }
            for my $other_view (values %flat_view) {
                $other_view->{canvas}->coords($other_view->{new_object_indicator}, 0,0,0,0);
            }
            for my $other_view (values %flat_view) {

                my @view_coords = model_xyz_to_view_xy($other_view, @{ $new_vertex->{vertex} });

                $other_view->{canvas}->coords($other_view->{selected_line_x}, $view_coords[0], 0, $view_coords[0], $view_height);
                $other_view->{canvas}->coords($other_view->{selected_line_y}, 0, $view_coords[1], $view_width, $view_coords[1]);
            }
        }
    }
    else {
        if ($new_vertex->{from_view}) {
            $log->debug("undo a");
            $new_vertex = {};
            for my $other_view (values %flat_view) {
                $other_view->{canvas}->coords($other_view->{selected_line_x}, 0,0,0,0);
                $other_view->{canvas}->coords($other_view->{selected_line_y}, 0,0,0,0);
            }
        }
    }
    queue_gl_draw();
    return;
}

################################################################################

sub
object_list_select_handler
{
    my ($object_lb) = @_;

    refresh_views();
    queue_gl_draw() if $gl_setting->{render_by_selection};
}

################################################################################

sub
change_view_snap
{
    my ($view, $x_inc, $y_inc) = @_;
    $log->debug("view $view, x_inc $x_inc, y_inc $y_inc");

    # convert the current snap settings back to model coords, adjust the model coords by
    # the specified amount, and convert back to view coords
    my @model_snap = view_xy_to_model_xyz($view, $view->{snap_x}, $view->{snap_y});

    $model_snap[ $view->{x_dim} ] += $x_inc;
    $model_snap[ $view->{y_dim} ] += $y_inc * $view->{y_axis_direction};

    ($view->{snap_x}, $view->{snap_y}) = model_xyz_to_view_xy($view, @model_snap);

    move_tracker($current_view->{canvas}, $current_view->{snap_x}, $current_view->{snap_y}, { no_snap => 1 } );
}

################################################################################

sub
get_key_factor 
{
    my $factor;
    if ($current_text =~ /^[.\d]+$/) {
        $factor = $current_text;
    }
    else {
        $factor = 1;
    }
    $factor *= 10 if $shift_key_down;
    $factor /= 10 if $control_key_down;
    return $factor;
}

################################################################################

sub
key_handler
{
    my ($entry, $keyname) = @_;

    # easy way of describing keys that just toggle flags
    my $flag_toggle_keys = {
        e => \$gl_setting->{eye_follows_cursor},
        l => \$gl_setting->{light_follows_cursor},
        t => \$gl_setting->{target_follows_cursor},
        S => \$filter_shown_by_position,
    };

    my $key_actions = {
        q => sub { $main_win->destroy; },
        n => sub {
            $current_text = '';
            $text_mode = $TM_NAME;
        },
        d => sub { $divisions = $divisions < 10 ? $divisions + 1 : 2; refresh_views(); },
        o => sub { 
            # o for origin
            for my $view (values %flat_view) {
                my @model_origin = (0,0,0);
                ($view->{snap_x}, $view->{snap_y}) = model_xyz_to_view_xy($view, @model_origin);
            }
            move_tracker($current_view->{canvas}, $current_view->{snap_x}, $current_view->{snap_y}, { no_snap => 1 } );
        },
        Escape => sub { $current_text = ""; $text_mode = $TM_NORMAL; },
        Return => sub {

            if ($text_mode == $TM_NAME) {
                # use the current text as the name for all selected objects
                if ($current_text) {
                    for my $index ($object_list->curselection()) {
                        $object_list->delete($index);
                        $object_list->insert($index, $current_text);
                        $object_list->selectionSet($index);
                    }
                    map { $things[$_]->{name} = $current_text; } update_selected_flags();
                }
                $text_mode = $TM_NORMAL;
                $current_text = "";
            }
            else {

                # fake a click at the current snap position
                click_tracker($current_view->{canvas}, 0, $current_view->{snap_x}, $current_view->{snap_y} );
                click_tracker($current_view->{canvas}, 1, $current_view->{snap_x}, $current_view->{snap_y} );
            }
        },
        Left => sub { my $f = get_key_factor(); change_view_snap ($current_view, -1*$f,0); },
        Right => sub { my $f = get_key_factor(); change_view_snap ($current_view, 1*$f,0); },
        Up => sub { my $f = get_key_factor(); change_view_snap ($current_view, 0,1*$f); },
        Down => sub { my $f = get_key_factor(); change_view_snap ($current_view, 0,-1*$f); },
        Delete => sub {
            for my $i (update_selected_flags()) {
                my $thing = $things[$i];
                splice @things, $i, 1;
            }
            for my $i (reverse $object_list->curselection()) {
                $object_list->delete($i);
            }
            update_listed_thing_indices();
            queue_gl_draw();
        },
        BackSpace => sub {
            $log->debug("backspace: $current_text");
            if (length($current_text)) {
                $current_text =~ s/.$//;
                $log->debug("backspaced: $current_text");
            }
        },
#        space => sub { $current_text .= ' ' if $text_mode == $TM_NAME; },
#        underscore => sub { $current_text .= '_' if $text_mode == $TM_NAME; },
        p => sub {
            for my $i (update_selected_flags()) {
                next if $things[$i]->{do_not_save};
                show_properties($things[$i]);
            }
        },

        # next drag action defines shown area
        s => sub { $drag_to_show_flag = ! $drag_to_show_flag; },
            
    };

    # add toggle actions
    for my $key (keys %{ $flag_toggle_keys } ) {
        $key_actions->{$key} = sub { ${ $flag_toggle_keys->{$key} } = ! ${ $flag_toggle_keys->{$key} } };
    }

    # change named printables into the real chars
    my %named_printables = (
        space => ' ',
        underscore => '_',
        period => '.',
    );
    $keyname = $named_printables{$keyname} if $named_printables{$keyname};

    my $mode_key_regex = {

        # everything fires in normal mode
        $TM_NORMAL => qr/./o,

        # special keys for name editing
        $TM_NAME => qr/Return|BackSpace|Escape|space|underscore/o,
    };

    # perform key actions if the mode supports that key
    if ((my $sub = $key_actions->{$keyname}) && $keyname =~ $mode_key_regex->{$text_mode}) {
        $log->debug("doing sub for $keyname");
        $sub->();
    }
    elsif ($keyname =~ /\A[.\d]\z/ || ($text_mode == $TM_NAME && $keyname =~ /^[\w\s_]$/)) {

        # in normal mode accumulate numbers only as input for the next action,
        # in name mode, use standard identifier chars
        $current_text .= $keyname;
        $log->debug("add char $keyname");
    }
    else {
        $log->debug("key $keyname");
    }

    # update status message according to mode
    if ($text_mode == $TM_NAME) {
        $msg_statusvar = 'Enter object name';
    }
    else {
        $msg_statusvar = '';
    }

    $status_entry->icursor('end');

    # hmmm
    if ($keyname =~ /\AS\z/) {
        refresh_views();
    }

}

################################################################################

sub
init_tk() 
{

	# generic list for checkbox control over a group of controls
	my @controls;

	$main_win = MainWindow->new(-background => 'black');
	$main_win->geometry("800x650+210+50");
    my @label_opts = ( -relief => 'sunken', -width => 5, -font => $small_font);

    my $status_frame = $main_win->Frame(-background => 'darkgreen')->pack(-fill => 'x', -side => 'bottom');
    $status_frame->Label(-text => "Cursor", -font => $small_bold_font)->pack(
        $status_frame->Label(-textvariable => \$cursor_statusvar[0], @label_opts),
        $status_frame->Label(-textvariable => \$cursor_statusvar[1], @label_opts),
        $status_frame->Label(-textvariable => \$cursor_statusvar[2], @label_opts),
        $status_frame->Label(-text => "Selected", -font => $small_bold_font),
        $status_frame->Label(-textvariable => \$selected_statusvar[0], @label_opts),
        $status_frame->Label(-textvariable => \$selected_statusvar[1], @label_opts),
        $status_frame->Label(-textvariable => \$selected_statusvar[2], @label_opts),
        $status_frame->Label(-text => "Divs", -font => $small_bold_font),
        $status_frame->Label(-textvariable => \$divisions, @label_opts),
        $status_entry = $status_frame->Entry(-textvariable => \$current_text, -relief => 'sunken', -width => 20, -background => 'white'),
        $status_frame->Label(-textvariable => \$msg_statusvar, -relief => 'sunken', -width => 80, -anchor => 'w'),
        -side => 'left', -expand => 1, -fill => 'both',
        );
        
    # we want custom keystroke handling for the status entry widget so we can 
    # react to arrows, enter, and so we control what is displayed instead of just dumping the keys pressed
    $status_entry->bindtags(undef);
	$status_entry->bind("<KeyPress>", [ \&key_handler, Ev('K') ] );

	$toolbar = $main_win->Frame->pack(-anchor => 'w', -fill => 'x', -expand => 0);

	$toolbar->Button(-takefocus => 0, -font => $small_bold_font,
			-text => "S", -command => sub { 
				if (Exists($source_win)) {
					$source_win->deiconify();
					$source_win->raise();
				}
				else {
					# create the source window
					$source_win = $main_win->Toplevel();
					$source_win->transient($main_win);
					$source_win->geometry("400x600+800+300");
					$source_win->title("POV Source");
					$source_win->Button( -text => "Hide", -command => sub { $source_win->withdraw })->grid(
						$source_win->Button( -text => "Render", -command => \&render_scene),
						$source_win->Button( -text => "Open", -command => \&open_pov_file),
						$source_win->Button( -text => "Save", -command => \&save_pov_file),
						-sticky => 'ew');

					my $source_nb = $source_win->NoteBook()->grid('-','-','-', -sticky => 'nsew');
					my $source_tab = $source_nb->add('source', -label => 'Source');
					$source_text = $source_tab->Scrolled("Text", -scrollbars => 'osoe',
						-width => 50, -wrap => 'none')->pack(-expand => 1, -fill => 'both');
					my $output_tab = $source_nb->add('output', -label => 'Output');
					$output_text = $output_tab->Scrolled("ROText", -scrollbars => 'osoe',
						-width => 50, -wrap => 'none')->pack(-expand => 1, -fill => 'both');

					$source_win->gridColumnconfigure(0, -weight => 1);
					$source_win->gridColumnconfigure(1, -weight => 1);
					$source_win->gridColumnconfigure(2, -weight => 1);
					$source_win->gridColumnconfigure(3, -weight => 1);
					$source_win->gridRowconfigure(1, -weight => 1);

					$current_source = get_povray_source($path_to_thing{''});
					$source_text->insert('end',$current_source);
				}
			 }
		)->pack(

		$toolbar->Button(-takefocus => 0, -font => $small_bold_font,
				-text => "L", -command => sub
			{ 
					if (Exists($line_win)) {
						$line_win->deiconify();
						$line_win->raise();
					}
					else {
						# create the lines window
						$line_win = $main_win->Toplevel();
						$line_win->transient($main_win);
						$line_win->geometry("300x300+600+200");
						$line_win->title("POV Lines");
						$line_win->Button( -text => "Hide", -command => sub { $line_win->withdraw })->grid(
							$line_win->Checkbutton(-text => "Add", -variable => \$add_on_line_click),
							$line_win->Checkbutton(-text => "Remove", -variable => \$remove_line_point),
							$line_win->Checkbutton(-text => "Smooth", -variable => \$smooth_line, -command => \&draw_line),
                            $line_win->NumEntry(
                                -width => 3, 
                                -font => $small_bold_font,
                                -minvalue => 0,
                                -maxvalue => 10,
                                -textvariable => \$splinesteps,
                                -browsecmd => sub { $log->debug("bc\n"); draw_line; },
                                -foreground => 'black',
                                ),
							-sticky => 'ew');
						$line_canvas = $line_win->Canvas->grid('-','-','-','-', -sticky => 'nsew');
						$line_canvas->CanvasBind('<ButtonPress-1>', [ \&line_click, Ev('x'), Ev('y'), 1 ]);
						$line_canvas->CanvasBind('<ButtonRelease-1>', [ \&line_click, Ev('x'), Ev('y'), 0 ]);
						$line_canvas->CanvasBind('<Motion>', [ \&move_line_point, Ev('x'), Ev('y') ]);
						$line_win->gridColumnconfigure(0, -weight => 1);
						$line_win->gridColumnconfigure(1, -weight => 1);
						$line_win->gridColumnconfigure(2, -weight => 1);
						$line_win->gridRowconfigure(1, -weight => 1);
					}
			 }),

		$toolbar->Button(-takefocus => 0, -font => $small_bold_font,
				-text => "R", -command => sub
			{
				render_scene(refresh_source => 1);
			}),

		$toolbar->Button(
            -takefocus => 0, 
            -font => $small_bold_font,
            -text => "Color", 
            -command => sub { 

                if ( my $color = $main_win->chooseColor( ) ) {
                    $log->debug("color = $color");
                    my @rgb = ($color =~ /([a-f0-9]{4})/g);
                    map { $_ = hex($_) } @rgb;
                    map { $_ /= 0xFFFF } @rgb;
                    $log->debug("rgb = " . Dumper(\@rgb));

                }

            },
        ),

		$toolbar->Button(
            -takefocus => 0, 
            -font => $small_bold_font,
            -text => "Test", 
            -command => sub { queue_gl_draw(); }
        ),

		$toolbar->Button(
            -takefocus => 0, 
            -font => $small_bold_font,
            -text => "Open", 
            -command => \&open_from_file,
        ),

		$toolbar->Button(
            -takefocus => 0, 
            -font => $small_bold_font,
            -text => "Save", 
            -command => \&save_to_file,
        ),

		$toolbar->Button(
            -takefocus => 0, 
            -font => $small_bold_font,
            -text => "Save As...", 
            -command => sub { $filename = undef; save_to_file(); },
        ),

		$toolbar->Button(
            -takefocus => 0, 
            -font => $small_bold_font,
            -text => "Clear", 
            -command => sub { clear_all(); $filename = undef; },
        ),

		$toolbar->Button(
            -takefocus => 0, 
            -font => $small_bold_font, 
			-text => 'Toolbox',
            -command => sub { $toolbox_win->deiconify(); $toolbox_win->raise(); },
        ),

        $toolbar->Button(
            -takefocus => 0, 
            -font => $small_bold_font,
            -text => 'Copy', 
            -command => [ \&copy_current ],
        ),

        $toolbar->Button(
            -takefocus => 0,
            -font => $small_bold_font,
            -text => "Delete",
            -command => [ \&delete_current ],
        ),

        $toolbar->Button(
            -takefocus => 0,
            -font => $small_bold_font,
            -text => "Debug",
            -command => sub { $debug = ! $debug; },
        ),

		-side => 'left');

	my $pw = $main_win->Panedwindow->pack(-side => 'top', -expand => 'yes', -fill => 'both');
	$control_frame = $pw->Frame(-relief => 'sunken', -borderwidth => 2);
	$view_frame = $pw->Frame(-relief => 'sunken', -borderwidth => 2, -background => 'white');
	$pw->add($control_frame);
	$pw->add($view_frame);

	# create the list
	$object_list = $control_frame->Scrolled('Listbox', -scrollbars => 'osoe',
        -takefocus => 0,
		-selectmode => 'extended',
		);

	$object_list->pack(-side => 'top', -fill => 'both', -expand => 1);
    $object_list->bind('<<ListboxSelect>>', \&object_list_select_handler);
    $object_list->bind('<Double-Button-1>',
        sub {
            for my $i (update_selected_flags()) {
                next if $things[$i]->{do_not_save};
                show_properties($things[$i]);
            }
        });

    $main_win->update;

    $toolbox_win = $main_win->Toplevel();
    $toolbox_win->transient($main_win);
    $toolbox_win->title("Toolbox");
    $toolbox_win->withdraw;
#    $toolbox_win->Button( -text => "Close", -command => sub { $toolbox_win->withdraw })->grid(-columnspan => 7, -sticky => 'ew');
    $toolbox_win->protocol('WM_DELETE_WINDOW', sub { $toolbox_win->withdraw });
    $toolbox_win->bind("<KeyPress>", [ \&key_handler, Ev('K') ] );
    my $toolbox_nb = $toolbox_win->NoteBook()->grid('-','-','-', -sticky => 'nsew');

	# 'options' settings 

    my $option_tab = $toolbox_nb->add('options', -label => 'Options');
	$option_tab->Checkbutton( 
		-text => 'Debug', 
        -variable => \$debug,
        %{ $default_widget_type_properties->{Checkbutton} },
        )->grid();
	$option_tab->Checkbutton( 
		-text => 'Gravity', 
        -variable => \$drop_unsupported,
        %{ $default_widget_type_properties->{Checkbutton} },
        )->grid();

	# 'move' settings 

    my $move_tab = $toolbox_nb->add('move', -label => 'Move');

	my $fr = $move_tab->Frame(-relief => 'raised', -borderwidth => 2)->grid('-');
	$fr->Label(-text => 'Order', -font => $small_bold_font)->grid('-','-');
	# create the order controls
	$move_asc_order{x} = $move_asc_order{y} = $move_asc_order{z} = 1;

	$move_order_lb = $fr->Listbox(-takefocus => 0, -height => 3 ,-width => 3, -font => $small_bold_font)->grid(
		-row => 1, -column => 0, -rowspan => 3);
	$move_order_lb->insert('end', qw(X Y Z));
	# note that this type of command binding automatically passes the lb ref as the first arg
	$move_order_lb->bind('<ButtonRelease-1>' => [ \&move_list_item_down ] );

	for (0..5) {
		my @text = ('X Asc', 'Desc', 'Y Asc', 'Desc', 'Z Asc', 'Desc');
		my @vars = (\$move_asc_order{x}, \$move_asc_order{y}, \$move_asc_order{z});
		my @vals = (1,0);
		$fr->Radiobutton(
			-takefocus => 0, 
			-text => $text[$_],
			-variable => $vars[int($_/2)],
			-font => $small_bold_font,
			-value => $vals[$_ % 2],
			)->grid(
				-row => int($_/2)+1,
				-column => ($_ % 2) + 1,
				);
	}

	foreach (qw(x y z)) {

		# create these controls first so the main cb can change them
		@controls = ();
		push @controls, $move_tab->Checkbutton(
			-text => 'Dimension',
			-variable => \$move_dim_flag{$_},
			-state => 'disabled',
            %{ $default_widget_type_properties->{Checkbutton} },
			);

		push @controls, $move_tab->NumEntry(
			-minvalue => -$GDIM{$_} + 1,
			-maxvalue => $GDIM{$_} - 1,
			-textvariable => \$move_start_amount{$_},
			-state => 'disabled',
            %{ $default_widget_type_properties->{NumEntry} },
			);

		push @controls, $move_tab->NumEntry(
			-minvalue => -$GDIM{$_} + 1,
			-maxvalue => $GDIM{$_} - 1,
			-textvariable => \$move_loop_amount{$_},
			-state => 'disabled',
            %{ $default_widget_type_properties->{NumEntry} },
			);

		# now create the controlling checkbox
		$move_tab->Checkbutton(
			-text => "Adjust " . uc($_),
			-variable => \$move_adjust_flag{$_},
			-command => [ \&control_switcher, \$move_adjust_flag{$_}, @controls ],
            %{ $default_widget_type_properties->{Checkbutton} },
			)->grid($controls[0], -sticky => 'w');

		# lay out the fields
		$move_tab->Label( -text => "Start", -font => $small_bold_font)->grid(
			$move_tab->Label( -text => "Loop", -font => $small_bold_font),
			);
		$controls[1]->grid(
			$controls[2],
		# 	-sticky => 'w'
			);
	}

	$move_tab->Button(
		-text => 'Move',
		-font => $small_bold_font,
		-takefocus => 0,
		-command => \&move_current)->grid('-');

	# enable column resizing
	$move_tab->gridColumnconfigure(0, -weight => 1);
	$move_tab->gridColumnconfigure(1, -weight => 1);
	# $move_tab->gridColumnconfigure(2, -weight => 1);

	# 'select' settings

    my $select_tab = $toolbox_nb->add('select', -label => 'Select');

	# create the position filter fields first so the checkbutton can turn them on & off
	@controls = ();
	foreach (0..5) {
		$select_pos_var[$_] = (0,$GX-1,0,$GY-1,0,$GZ-1)[$_];
		# no command callbacks, we aren't changing the current selection
		push @controls, $select_tab->NumEntry(
			-minvalue => 0,
			-maxvalue => $_ == 0 ? $GX-1 : $_ == 1 ? $GY-1 : $GZ-1,
			-textvariable => \$select_pos_var[$_],
			-state => 'disabled',
            %{ $default_widget_type_properties->{NumEntry} },
			);
	}
	# create the checkbutton which enables/disables the fields
	$select_tab->Checkbutton(
		-text => 'Position',
		-variable => \$select_pos_flag,
		-command => [ \&control_switcher, \$select_pos_flag, @controls ],
        %{ $default_widget_type_properties->{Checkbutton} },
		)->grid(
			$select_tab->Label(-text => 'Min', -font => $small_bold_font),
			$select_tab->Label(-text => 'Max', -font => $small_bold_font),
			);
	# now lay out the fields
	$select_tab->Label(-text => 'X', -font => $small_bold_font)->grid($controls[0], $controls[1], -sticky => 'e');
	$select_tab->Label(-text => 'Y', -font => $small_bold_font)->grid($controls[2], $controls[3], -sticky => 'e');
	$select_tab->Label(-text => 'Z', -font => $small_bold_font)->grid($controls[4], $controls[5], -sticky => 'e');

	# create the size filter fields first so the checkbutton can turn them on & off
	@controls = ();
	foreach (0..5) {
		$select_size_var[$_] = (1,$GX,1,$GY,1,$GZ)[$_];
		# no command callbacks, we aren't changing the current selection
		push @controls, $select_tab->NumEntry(
			-minvalue => 1,
			-maxvalue => $_ == 0 ? $GX : $_ == 1 ? $GY : $GZ,
			-textvariable => \$select_size_var[$_],
			-state => 'disabled',
            %{ $default_widget_type_properties->{NumEntry} },
			);
	}
	# create the checkbutton which enables/disables the fields
	my $cb = $select_tab->Checkbutton(
		-text => 'Size',
		-variable => \$select_size_flag,
		-command => [ \&control_switcher, \$select_size_flag, @controls ],
        %{ $default_widget_type_properties->{Checkbutton} },
		)->grid(
		$select_tab->Label(-text => 'Min', -font => $small_bold_font),
		$select_tab->Label(-text => 'Max', -font => $small_bold_font),
		);
	# re-grid the cb to position it correctly
	$cb->grid(-sticky => 'w');
	# now lay out the fields
	$select_tab->Label(-text => 'X', -font => $small_bold_font)->grid($controls[0], $controls[1], -sticky => 'e');
	$select_tab->Label(-text => 'Y', -font => $small_bold_font)->grid($controls[2], $controls[3], -sticky => 'e');
	$select_tab->Label(-text => 'Z', -font => $small_bold_font)->grid($controls[4], $controls[5], -sticky => 'e');

	# create a toggled menubutton for the material list
	my $tm = togglemenu($select_tab, "Material", \%select_material, \&printargs, qw(Mat1 Mat2 Mat3));
	$tm->configure(-state => 'disabled');
	# create the checkbutton which enables/disables the menu
	$select_tab->Checkbutton(
		-text => 'Material',
		-variable => \$select_material_flag,
		-command => [ \&control_switcher, \$select_material_flag, $tm ],
        %{ $default_widget_type_properties->{Checkbutton} },
		)->grid($tm, "-");

	# create a big 'select all' button
	$select_tab->Button(
		-text => 'Select All',
		-font => $small_bold_font,
		-command => sub { add_to_current_group(@things) },
		)->grid('-','-');

	# 'show' settings

    my $show_tab = $toolbox_nb->add('show', -label => 'Show');

	# create the position filter fields first so the checkbutton can turn them on & off
	@controls = ();
	foreach (0 .. 2) {
		push @controls, $show_tab->NumEntry(
			-textvariable => \$show_filter_pos_min[$_],
			-state => 'disabled',
            -browsecmd => \&queue_gl_draw,
            -command => \&queue_gl_draw,
            %{ $default_widget_type_properties->{NumEntry} },
			);
	}
	foreach (0 .. 2) {
		push @controls, $show_tab->NumEntry(
			-textvariable => \$show_filter_pos_max[$_],
			-state => 'disabled',
            -browsecmd => \&queue_gl_draw,
            -command => \&queue_gl_draw,
            %{ $default_widget_type_properties->{NumEntry} },
			);
	}

	push @controls, $show_tab->Checkbutton(
		-text => 'Filter GL view',
		-variable => \$filter_gl_view,
		-command => \&queue_gl_draw,
        %{ $default_widget_type_properties->{Checkbutton} },
		);

	push @controls, $show_tab->Checkbutton(
		-text => 'Include object dimension',
		-variable => \$show_filter_includes_dimension,
		-command => \&queue_gl_draw,
        %{ $default_widget_type_properties->{Checkbutton} },
		);

	push @controls, $show_tab->Checkbutton(
		-text => 'Filter by intersection',
		-variable => \$filter_by_intersection,
		-command => \&queue_gl_draw,
        %{ $default_widget_type_properties->{Checkbutton} },
		);

	# create the checkbutton which enables/disables the fields
	$show_tab->Checkbutton(
		-text => 'Position',
		-variable => \$filter_shown_by_position,
		-command => [ \&control_switcher, \$filter_shown_by_position, @controls ],
        %{ $default_widget_type_properties->{Checkbutton} },
		)->grid(
			$show_tab->Label(-text => 'Min', -font => $small_bold_font),
			$show_tab->Label(-text => 'Max', -font => $small_bold_font),
			);
	# now lay out the fields
	$show_tab->Label(-text => 'X', -font => $small_bold_font)->grid($controls[0], $controls[3], -sticky => 'e');
	$show_tab->Label(-text => 'Y', -font => $small_bold_font)->grid($controls[1], $controls[4], -sticky => 'e');
	$show_tab->Label(-text => 'Z', -font => $small_bold_font)->grid($controls[2], $controls[5], -sticky => 'e');
    $controls[6]->grid();
    $controls[7]->grid();
    $controls[8]->grid();

#	# create the size filter fields first so the checkbutton can turn them on & off
#	@controls = ();
#	foreach (0..5) {
#		$show_size_var[$_] = (1,$GX,1,$GY,1,$GZ)[$_];
#		# no command callbacks, we aren't changing the current showion
#		push @controls, $show_tab->NumEntry(
#			-minvalue => 1,
#			-maxvalue => $_ == 0 ? $GX : $_ == 1 ? $GY : $GZ,
#			-textvariable => \$show_size_var[$_],
#			-state => 'disabled',
#            %{ $default_widget_type_properties->{NumEntry} },
#			);
#	}
#	# create the checkbutton which enables/disables the fields
#	my $cb = $show_tab->Checkbutton(
#		-text => 'Size',
#		-variable => \$show_size_flag,
#		-command => [ \&control_switcher, \$show_size_flag, @controls ],
#        %{ $default_widget_type_properties->{Checkbutton} },
#		)->grid(
#		$show_tab->Label(-text => 'Min', -font => $small_bold_font),
#		$show_tab->Label(-text => 'Max', -font => $small_bold_font),
#		);
#	# re-grid the cb to position it correctly
#	$cb->grid(-sticky => 'w');
#	# now lay out the fields
#	$show_tab->Label(-text => 'X', -font => $small_bold_font)->grid($controls[0], $controls[1], -sticky => 'e');
#	$show_tab->Label(-text => 'Y', -font => $small_bold_font)->grid($controls[2], $controls[3], -sticky => 'e');
#	$show_tab->Label(-text => 'Z', -font => $small_bold_font)->grid($controls[4], $controls[5], -sticky => 'e');

	# 'gl' settings

    my $opengl_tab = $toolbox_nb->add('opengl', -label => 'OpenGL');

	$opengl_tab->Checkbutton(
		-text => 'Up vector above Eye',
		-variable => \$gl_setting->{up_vector_above_eye},
        %{ $default_widget_type_properties->{Checkbutton} },
		)->grid('-','-','-', -sticky => 'w');

	$opengl_tab->Checkbutton(
		-text => 'Eye follows cursor',
		-variable => \$gl_setting->{eye_follows_cursor},
        %{ $default_widget_type_properties->{Checkbutton} },
		)->grid('-','-','-', -sticky => 'w');

	$opengl_tab->Checkbutton(
		-text => 'Light follows cursor',
		-variable => \$gl_setting->{light_follows_cursor},
        %{ $default_widget_type_properties->{Checkbutton} },
		)->grid('-','-','-', -sticky => 'w');

	$opengl_tab->Checkbutton(
		-text => 'Target follows cursor',
		-variable => \$gl_setting->{target_follows_cursor},
        %{ $default_widget_type_properties->{Checkbutton} },
		)->grid('-','-','-', -sticky => 'w');

    # XYZ vector OpenGL settings
	foreach my $setting (keys %{ $gl_var }) {
        @controls = ();
        foreach my $element (sort keys %{ $gl_var->{$setting} }) {
            push @controls, $opengl_tab->NumEntry(
                -textvariable => \$gl_var->{$setting}->{$element},
                -browsecmd => \&queue_gl_draw,
                -command => \&queue_gl_draw,
                -increment => $gl_increment->{$setting}->{$element},
                %{ $default_widget_type_properties->{NumEntry} },
                );
        } 
        my $label = ucfirst $setting . " (" . join(',', map(ucfirst, sort keys %{ $gl_var->{$setting} })) . ")";
        $opengl_tab->Label(-text => $label, -font => $small_bold_font)->grid('-', '-', -sticky => 'w');
        my $first = shift @controls;
        $first->grid(@controls);
    }

    # rendering selected items for GL view
    # these controls change the effect of selection rather the selection itself, but the selection tab is still the best place for them
    $select_tab->Label(-text => "3D View", -font => $small_bold_font)->grid(
        $select_tab->Checkbutton(
            -text => 'Render',
            -variable => \$gl_setting->{render_by_selection},
            -command => [ \&queue_gl_draw ],
            %{ $default_widget_type_properties->{Checkbutton} },
            ),
        $select_tab->Checkbutton(
            -text => 'Invert',
            -variable => \$gl_setting->{render_unselected},
            -command => [ \&queue_gl_draw ],
            %{ $default_widget_type_properties->{Checkbutton} },
            ),
    );

	# create initial views
	$flat_view{XY}->{canvas} = $view_frame->Canvas( -background => 'darkgrey', -takefocus => 0, -cursor => 'dotbox')->grid(
		$flat_view{YZ}->{canvas} = $view_frame->Canvas( -background => 'darkgrey', -takefocus => 0, -cursor => 'dotbox'),
		-sticky => 'nsew');
	$flat_view{XZ}->{canvas} = $view_frame->Canvas( -background => 'darkgrey', -takefocus => 0, -cursor => 'dotbox')->grid(
        $gl_frame = $view_frame->Frame(-height => 20, -width => 20, -background => 'red'),
		-sticky => 'nsew');

	# make the views autosize
	my ($columns, $rows) = $view_frame->gridSize();
	for (my $i = 0; $i < $columns; $i++) {
		$view_frame->gridColumnconfigure($i, -weight => 1);
	}
	for (my $i = 0; $i < $rows; $i++) {
		$view_frame->gridRowconfigure($i, -weight => 1);
	}

    # remove default bindings (mouse wheel scrolls up & down)
#    $flat_view{XY}->{canvas}->bindtags(undef);
#    $flat_view{XZ}->{canvas}->bindtags(undef);
#    $flat_view{YZ}->{canvas}->bindtags(undef);

	# do work for each view
	foreach my $view_name (keys %flat_view) {
		my $view = $flat_view{$view_name};
		$view->{title} = $view_name;
		my $canvas = $view->{canvas};

        # remove default bindings (mouse wheel scrolls up & down)
        $canvas->bindtags(undef);

        # map from canvas to view
        $canvas_view{$canvas} = $view;

		# create a rectangle to use when selecting multiple objects
		$view->{selection_box_id} = $canvas->createRectangle(0,0,100,100,
			-state => 'hidden', -tags => 'selection', -outline => 'SpringGreen2'),

		# selection lines
		$view->{selection_line_x} = $canvas->createLine(0,0,0,0,
            -tags => 'selection',
            -fill => 'SpringGreen2'),
		$view->{selection_line_y} = $canvas->createLine(0,0,0,0,
            -tags => 'selection',
            -fill => 'SpringGreen2'),

		# selected lines
		$view->{selected_line_x} = $canvas->createLine(0,0,0,0,
            -tags => 'selection',
            -fill => 'gold'),
		$view->{selected_line_y} = $canvas->createLine(0,0,0,0,
            -tags => 'selection',
            -fill => 'gold'),

        # new object
		$view->{new_object_indicator} = $canvas->createRectangle(0,0,0,0,
            -tags => 'selection',
            -outline => 'green'),

		# mouse-wheel moves selection up & down
		$canvas->CanvasBind("<ButtonPress-4>", [ \&zoom_tracker, -1 ] );
		$canvas->CanvasBind("<ButtonPress-5>", [ \&zoom_tracker, 1 ] );

		# select thing or start multiple selection
		$canvas->CanvasBind("<ButtonPress-1>", [ \&click_tracker, 0, Ev('x'), Ev('y') ] );
		$canvas->CanvasBind("<ButtonRelease-1>", [ \&click_tracker, 1, Ev('x'), Ev('y') ] );
#		$canvas->CanvasBind("<ButtonPress-2>", [ \&click_tracker, 2, Ev('x'), Ev('y') ] );
		$canvas->CanvasBind("<ButtonPress-3>", [ \&undo_last_click, 3, Ev('x'), Ev('y') ] );

		# move selected thing or continue multiple selection 
		$canvas->CanvasBind("<Button1-Motion>", [ \&drag_tracker, 1, Ev('x'), Ev('y') ] );
		# finish multiple selection
#		$canvas->CanvasBind("<ButtonRelease-1>", [ \&drag_tracker, 0, Ev('x'), Ev('y') ] );

		$canvas->CanvasBind("<Motion>", [ \&move_tracker, Ev('x'), Ev('y'), {} ] );

        # these controls change the effect of selection rather the selection itself, but the selection tab is still the best place for them
        $view->{render_by_selection} = 0;
        $view->{render_unselected} = 0;
        $select_tab->Label(-text => "View $view_name", -font => $small_bold_font)->grid(
            $select_tab->Checkbutton(
                -text => 'Control Render',
                -variable => \$view->{render_by_selection},
                -command => [ \&refresh_views ],
                %{ $default_widget_type_properties->{Checkbutton} },
                ),
            $select_tab->Checkbutton(
                -text => 'Invert',
                -variable => \$view->{render_unselected},
                -command => [ \&refresh_views ],
                %{ $default_widget_type_properties->{Checkbutton} },
                ),
        );

	}

	$main_win->bind("<KeyPress-Shift_L>" => sub { $shift_key_down |= 0x01; $log->debug("shift_key_down now $shift_key_down\n") if $debug; } );
	$main_win->bind("<KeyPress-Shift_R>" => sub { $shift_key_down |= 0x02; $log->debug("shift_key_down now $shift_key_down\n") if $debug; } );
	$main_win->bind("<KeyRelease-Shift_L>" => sub { $shift_key_down &= ~0x01; $log->debug("shift_key_down now $shift_key_down\n") if $debug; } );
	$main_win->bind("<KeyRelease-Shift_R>" => sub { $shift_key_down &= ~0x02; $log->debug("shift_key_down now $shift_key_down\n") if $debug; } );

	$main_win->bind("<KeyPress-Control_L>" => sub { $control_key_down |= 0x01; $log->debug("control_key_down now $control_key_down\n") if $debug; } );
	$main_win->bind("<KeyPress-Control_R>" => sub { $control_key_down |= 0x02; $log->debug("control_key_down now $control_key_down\n") if $debug; } );
	$main_win->bind("<KeyRelease-Control_L>" => sub { $control_key_down &= ~0x01; $log->debug("control_key_down now $control_key_down\n") if $debug; } );
	$main_win->bind("<KeyRelease-Control_R>" => sub { $control_key_down &= ~0x02; $log->debug("control_key_down now $control_key_down\n") if $debug; } );

	$main_win->bind("<KeyPress-Alt_L>" => sub { $alt_key_down |= 0x01; $log->debug("alt_key_down now $alt_key_down\n") if $debug; } );
	$main_win->bind("<KeyPress-Alt_R>" => sub { $alt_key_down |= 0x02; $log->debug("alt_key_down now $alt_key_down\n") if $debug; } );
	$main_win->bind("<KeyRelease-Alt_L>" => sub { $alt_key_down &= ~0x01; $log->debug("alt_key_down now $alt_key_down\n") if $debug; } );
	$main_win->bind("<KeyRelease-Alt_R>" => sub { $alt_key_down &= ~0x02; $log->debug("alt_key_down now $alt_key_down\n") if $debug; } );

    $main_win->update;
    $status_entry->focus;

    return;
}

################################################################################

sub SetMaterial {
  my ($r_spec, $r_amb, $r_diff, $r_shin) = @_;
  glMaterialfv_p(GL_FRONT, GL_SPECULAR, @$r_spec);
  glMaterialfv_p(GL_FRONT, GL_SHININESS, @$r_shin);
  glMaterialfv_p(GL_FRONT, GL_AMBIENT, @$r_amb);
  glMaterialfv_p(GL_FRONT, GL_DIFFUSE, @$r_diff);
}

################################################################################

sub Box {
  my ($width, $height, $depth, $solid)= @_;
  my $i;
  my $j = 0;
  my $x = $width / 2.0;
  my $y = $height / 2.0;
  my $z = $depth / 2.0;

  for ($i = 0; $i < 4; $i++) {
    glRotatef(90.0, 0.0, 0.0, 1.0);
    if ($j) {
      if (!$solid) {
        glBegin(GL_LINE_LOOP);
      }
      else {
        glBegin(GL_QUADS);
      }
      glNormal3f(-1.0, 0.0, 0.0);
      glVertex3f(-$x, $y, $z);
      glVertex3f(-$x, -$y, $z);
      glVertex3f(-$x, -$y, -$z);
      glVertex3f(-$x, $y, -$z);
      glEnd();
      if ($solid) {
        glBegin(GL_TRIANGLES);
        glNormal3f(0.0, 0.0, 1.0);
        glVertex3f(0.0, 0.0, $z);
        glVertex3f(-$x, $y, $z);
        glVertex3f(-$x, -$y, $z);
        glNormal3f(0.0, 0.0, -1.0);
        glVertex3f(0.0, 0.0, -$z);
        glVertex3f(-$x, -$y, -$z);
        glVertex3f(-$x, $y, -$z);
        glEnd();
      }
      $j = 0;
    }
    else {
      if (!$solid) {
        glBegin(GL_LINE_LOOP);
      }
      else {
        glBegin(GL_QUADS);
      }
      glNormal3f(-1.0, 0.0, 0.0);
      glVertex3f(-$y, $x, $z);
      glVertex3f(-$y, -$x, $z);
      glVertex3f(-$y, -$x, -$z);
      glVertex3f(-$y, $x, -$z);
      glEnd();
      if ($solid) {
        glBegin(GL_TRIANGLES);
        glNormal3f(0.0, 0.0, 1.0);
        glVertex3f(0.0, 0.0, $z);
        glVertex3f(-$y, $x, $z);
        glVertex3f(-$y, -$x, $z);
        glNormal3f(0.0, 0.0, -1.0);
        glVertex3f(0.0, 0.0, -$z);
        glVertex3f(-$y, -$x, -$z);
        glVertex3f(-$y, $x, -$z);
        glEnd();
      }
      $j = 1;
    }
  }
}

################################################################################

sub myBox {
    my ($width, $height, $depth, $solid)= @_;
    my $x = $width / 2.0;
    my $y = $height / 2.0;
    my $z = $depth / 2.0;

    if (!$solid) {
        glBegin(GL_LINE_LOOP);
    }
    else {
        glBegin(GL_QUADS);
    }

    # front
    glNormal3f(0.0, 0.0, 1.0);
    glVertex3f(-$x,-$y,$z);
    glVertex3f(-$x,$y,$z);
    glVertex3f($x,$y,$z);
    glVertex3f($x,-$y,$z);

    # back
    glNormal3f(0.0, 0.0, -1.0);
    glVertex3f(-$x,-$y,-$z);
    glVertex3f($x,-$y,-$z);
    glVertex3f($x,$y,-$z);
    glVertex3f(-$x,$y,-$z);

    # right
    glNormal3f(1.0, 0.0, 0.0);
    glVertex3f($x,-$y,$z);
    glVertex3f($x,$y,$z);
    glVertex3f($x,$y,-$z);
    glVertex3f($x,-$y,-$z);

    # left
    glNormal3f(-1.0, 0.0, 0.0);
    glVertex3f(-$x,-$y,$z);
    glVertex3f(-$x,-$y,-$z);
    glVertex3f(-$x,$y,-$z);
    glVertex3f(-$x,$y,$z);

    # top
    glNormal3f(0.0, 1.0, 0.0);
    glVertex3f($x,$y,$z);
    glVertex3f($x,$y,-$z);
    glVertex3f(-$x,$y,-$z);
    glVertex3f(-$x,$y,$z);

    # bottom
    glNormal3f(1.0, -1.0, 0.0);
    glVertex3f($x,-$y,$z);
    glVertex3f(-$x,-$y,$z);
    glVertex3f(-$x,-$y,-$z);
    glVertex3f($x,-$y,-$z);

    glEnd();

}

################################################################################

sub
color_vec_to_tk
{
    my $color_vec = shift;

    return sprintf "#%04x%04x%04x",
        ($color_vec->[0] / 1) * 0xFFFF,
        ($color_vec->[1] / 1) * 0xFFFF,
        ($color_vec->[2] / 1) * 0xFFFF,
        ;
}

################################################################################

sub
recalc_views
{

    # we assume that all frames are the same dimension including the gl frame
    $view_width = $gl_frame->width;
    $view_height = $gl_frame->height;

    my $aspect_ratio = $view_width / $view_height;

    for my $view (values %flat_view) {

        $view->{canvas}->configure(-background => $view->{bg} );
        $view->{canvas}->createText(10, 10, -anchor => 'nw', -text => $view->{name});

        # work out zoom
        if ($view_width < $view_height) {

            # more room on x axis, so y axis is limiting factor
            $view->{calc_x_offset} = ($view->{x_centre} - $view->{size} / 2) * $aspect_ratio;
            $view->{calc_y_offset} = $view->{y_centre} - $view->{size} / 2;
            $view->{size_factor} = $view_height / $view->{size};
        }
        else {
            $view->{calc_x_offset} = $view->{x_centre} - $view->{size} / 2;
            $view->{calc_y_offset} = ($view->{y_centre} - $view->{size} / 2) / $aspect_ratio;
            $view->{size_factor} = $view_width / $view->{size};
        }

        $log->debug("view $view->{name}, x & y centre = $view->{x_centre}, $view->{y_centre}, x & y offsets = $view->{calc_x_offset}, $view->{calc_y_offset},  size factor = $view->{size_factor}");
    }

}

################################################################################

sub
view_xy_to_model_xyz
{
    my ($view, $canvas_x, $canvas_y) = @_;
    $log->debug("view_xy_to_model_xyz: view, x, y = $view, $canvas_x, $canvas_y");

    confess "no size factor" unless $view->{size_factor};

    my @model_coords;

    $model_coords[ $view->{x_dim} ] = sprintf "%.$view->{precision}f", 
        ((($view_width - $canvas_x)/ $view->{size_factor}) + $view->{calc_x_offset}) / $view->{x_axis_direction};
    $log->debug(sprintf "x_dim = %d, width = %d, cao = %d, xad = %d",  $model_coords[ $view->{x_dim} ], $view_width,  $view->{calc_x_offset}, $view->{x_axis_direction} );
    $model_coords[ $view->{y_dim} ] = sprintf "%.$view->{precision}f", 
        ((($view_height - $canvas_y)/ $view->{size_factor}) + $view->{calc_y_offset}) / $view->{y_axis_direction};

    return @model_coords;
}

################################################################################

sub
model_xyz_to_view_xy
{
    my ($view, @model_coords) = @_;
#    $log->debug("model_xyz_to_view_xy: $view->{name}, model_coords = @model_coords");

    my $view_x = defined($model_coords[ $view->{x_dim} ])
        ? $view_width - ((($model_coords[ $view->{x_dim} ] * $view->{x_axis_direction}) - $view->{calc_x_offset}) * $view->{size_factor} )
        : 0;
    my $view_y = defined($model_coords[ $view->{y_dim} ])
        ? $view_height - ((($model_coords[ $view->{y_dim} ] * $view->{y_axis_direction}) - $view->{calc_y_offset}) * $view->{size_factor} )
        : 0;

    return ($view_x, $view_y);
}

################################################################################

sub
add_snap_target
{
    my ($view, $x, $y) = @_;
    $view->{canvas}->createOval($x - $snap_radius, $y - $snap_radius, $x + $snap_radius, $y + $snap_radius, -tags => [ 'model' ]);

    push @{ $view->{snaps} }, [ $x, $y ];

    return;
}

################################################################################

sub
thing_passes_filter
{
    my $thing = shift;

    return 1 unless $thing->{vert};

    my $show = 1;
    for my $dim (0 .. 2) {
        my $max = $thing->{vert}->[$dim];
        $max += $thing->{sizes}->[$dim] if $show_filter_includes_dimension;
        $show = $filter_by_intersection
            ? ($thing->{vert}->[$dim] <= $show_filter_pos_max[$dim]
                || $max <= $show_filter_pos_min[$dim])
            : ($thing->{vert}->[$dim] >= $show_filter_pos_min[$dim]
                && $max <= $show_filter_pos_max[$dim]);
        last unless $show;
    }

    return $show;
}

################################################################################

sub
refresh_views
{

    for my $view (values %flat_view) {
        $view->{canvas}->delete('model');
        $view->{snaps} = [];
        return unless defined $view->{size_factor};
        my $new_vertex_incomplete = 0;
        if ($new_vertex->{vertex}) {
            $new_vertex_incomplete = scalar grep { ! defined $_ } @{ $new_vertex->{vertex} };
        }
        if ($new_vertex_incomplete) {

            my @view_coords = model_xyz_to_view_xy($view, @{ $new_vertex->{vertex} } );

            $view->{canvas}->coords($view->{selected_line_x}, $view_coords[0], 0, $view_coords[0], $view_height);
            $view->{canvas}->coords($view->{selected_line_y}, 0, $view_coords[1], $view_width, $view_coords[1]);
        }
        else {
            $view->{canvas}->coords($view->{selected_line_x}, 0,0,0,0);
            $view->{canvas}->coords($view->{selected_line_y}, 0,0,0,0);
            $view->{canvas}->coords($view->{new_object_indicator}, 0,0,0,0);
        }
    }

    # update the selected flags in @things
    update_selected_flags();

    for my $thing (@system_things, @things) {

#        $log->debug("drawing $thing->{name}: ". Dumper($thing));

        for my $view (values %flat_view) {

            if ($view->{render_by_selection}) {
                if ($view->{render_unselected}) {
                    next if $thing->{_selected};
                }
                else {
                    next unless $thing->{_selected};
                }
            }

            if ($filter_shown_by_position) {
                next unless thing_passes_filter($thing);
            }

            my $xdim = $view->{x_dim};
            my $ydim = $view->{y_dim};

            my $color;
            if ($thing->{_selected}) {
                $color = 'white';
            }
            else {
                $color = color_vec_to_tk($thing->{color});
            }

            if ($thing->{type} == $OT_LINE) {

                my @view_verts;
                for my $vert ( @{ $thing->{verts} } ) {
                    my ($view_x, $view_y) = model_xyz_to_view_xy($view, @{ $vert });
                    push @view_verts, $view_x, $view_y;
                }
#                $log->debug("line at @view_verts");
                $thing->{_tk}->{$view->{name}} = $view->{canvas}->createLine(@view_verts, -fill => color_vec_to_tk($thing->{color}), -tags => 'model' );
            }
            elsif ($thing->{type} == $OT_CUBOID) {
                next unless $thing->{solid};
                # cuboids have a single vertex and a size array
#                $log->debug("cuboid at " . join('.',@{ $thing->{vert} }) . " size " . join('.',@{ $thing->{sizes} }) );
                my ($base_x, $base_y) = model_xyz_to_view_xy($view, @{ $thing->{vert} });
                my @model_corner = (
                    $thing->{vert}->[0] + $thing->{sizes}->[0],
                    $thing->{vert}->[1] + $thing->{sizes}->[1],
                    $thing->{vert}->[2] + $thing->{sizes}->[2],
                    );
                my ($corner_x, $corner_y) = model_xyz_to_view_xy($view, @model_corner);
                $thing->{_tk}->{$view->{name}} = $view->{canvas}->createRectangle(
                    $base_x, $base_y, $corner_x, $corner_y,
                    -outline => $color, -tags => 'model' );

                if ($thing->{_selected}) {
                    add_snap_target($view, $base_x, $base_y);
                    add_snap_target($view, $base_x, $corner_y);
                    add_snap_target($view, $corner_x, $corner_y);
                    add_snap_target($view, $corner_x, $base_y);
                }

            }
            elsif ($thing->{type} == $OT_RAMP || $thing->{type} == $OT_STAIRS || $thing->{type} == $OT_PRISM ) {

                my $calc_props = $thing->{_transform_props}->{$thing->{transform_type}};

                # transformed objects; use the end_vertices arrays to show the shape of the thing
                if ($thing->{transform_props}->{$thing->{transform_type}}->{Plane} eq $view->{name}) {
                    my @base_points;
                    my $havent_skipped = 1;
                    for my $vertex (@{ $calc_props->{end_vertices}->[0] }) {

                        # the first point in a prism is the centre
                        if ($thing->{type} == $OT_PRISM && $havent_skipped) {
                            $havent_skipped = 0;
                            next;
                        }

                        # note that all render-ready data must be translated by the thing's vertex.
                        my ($x,$y) = model_xyz_to_view_xy($view,
                            $vertex->[0] + $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,
                            $vertex->[1] + $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,
                            $vertex->[2] + $thing->{vert}->[2] + $thing->{sizes}->[2] / 2,
                            );
                        push @base_points, ($x, $y);
                    }
                    $thing->{_tk}->{$view->{name}} = $view->{canvas}->createPolygon(
                        @base_points,
                        -outline => $color, -fill => undef, -tags => 'model' );
                    if ($thing->{_selected}) {
                        while (@base_points) {
                            my $x = shift @base_points;
                            my $y = shift @base_points;
                            add_snap_target($view, $x, $y);
                        }
                    }
                }
                else {
                    my $from = $calc_props->{side_points}->[0];
                    my @snaps = ();
                    for my $index ( 1 .. $#{ $calc_props->{side_normals} } ) {

                        my $to = $calc_props->{side_points}->[$index * 2 + 1];

                        my ($from_x,$from_y) = model_xyz_to_view_xy($view,
                            $from->[0] + $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,
                            $from->[1] + $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,
                            $from->[2] + $thing->{vert}->[2] + $thing->{sizes}->[2] / 2,
                            );
                        my ($to_x,$to_y) = model_xyz_to_view_xy($view,
                            $to->[0] + $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,
                            $to->[1] + $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,
                            $to->[2] + $thing->{vert}->[2] + $thing->{sizes}->[2] / 2,
                            );

                        $view->{canvas}->createRectangle(
                            $from_x, $from_y, $to_x, $to_y,
                            -outline => $color, -tags => 'model' );

                        $from = $calc_props->{side_points}->[$index * 2];

                        if ($thing->{_selected}) {
                            add_snap_target($view, $from_x, $from_y);
                            add_snap_target($view, $to_x, $to_y);
                        }

                    }
                }

            }

            
        }
    }

    for my $view (values %flat_view) {
        $view->{canvas}->lower('model','selection');
    }

    return;
}

################################################################################

my $pending = 0;

sub
gl_draw
{

#    $log->debug("drawing");

    refresh_views();

    if ($gl_setting->{up_vector_above_eye}) {
        $gl_var->{up}->{x} = $gl_var->{eye}->{x} - $gl_var->{target}->{x};
        $gl_var->{up}->{y} = $gl_var->{eye}->{y} + 10;
        $gl_var->{up}->{z} = $gl_var->{eye}->{z} - $gl_var->{target}->{z};
    }

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    gluPerspective(
        $gl_var->{perspective}->{angle}, 
        $gl_frame->width / $gl_frame->height, 
        $gl_var->{perspective}->{'close'},
        $gl_var->{perspective}->{far});

    glMatrixMode(GL_MODELVIEW);

    glLoadIdentity();   #  clear the matrix

    glLightfv (GL_LIGHT0, GL_POSITION, pack 'f4', @{$gl_var->{light}}{qw/x y z/}, 0.0 );

    #  viewing transformation
    gluLookAt(
        @{$gl_var->{eye}}{qw/x y z/},
        @{$gl_var->{target}}{qw/x y z/},
        @{$gl_var->{up}}{qw/x y z/},
        );

    #  modeling transformation
    glScalef(@{$gl_var->{scale}}{qw/x y z/});

    glColor3f(1.0, 1.0, 0.0);

    for my $thing (@system_things, @things) {

#        $log->debug("rendering $thing->{name}");

        next if $thing->{no_gl_render};

        # we've already called refresh_views() and that updates the selected flags so we don't need to
        if ($gl_setting->{render_by_selection}) {
            if ($gl_setting->{render_unselected}) {
                next if $thing->{_selected};
            }
            else {
                next unless $thing->{_selected};
            }
        }

        if ($filter_shown_by_position && $filter_gl_view) {
            next unless thing_passes_filter($thing);
        }

        glColor3f(@{$thing->{color}});

        if ($thing->{type} == $OT_LINE) {
            glBegin(GL_LINES);
            glVertex3f(@{$thing->{verts}->[0]});
            glVertex3f(@{$thing->{verts}->[1]});
            glEnd();
        }
        elsif ($thing->{type} == $OT_QUAD) {
            glBegin(GL_QUADS);
            glVertex3f(@{$thing->{verts}->[0]});
            glVertex3f(@{$thing->{verts}->[1]});
            glVertex3f(@{$thing->{verts}->[2]});
            glVertex3f(@{$thing->{verts}->[3]});
            glEnd();
        }
        elsif ($thing->{type} == $OT_CUBOID) {
            glPushMatrix();
            glTranslatef( 
                $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,  
                $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,  
                $thing->{vert}->[2] + $thing->{sizes}->[2] / 2 );
            myBox(@{ $thing->{sizes} }, $thing->{solid});
            glPopMatrix();
        }
        elsif ($thing->{type} == $OT_PRISM) {
            glPushMatrix();
            glTranslatef( 
                $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,  
                $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,  
                $thing->{vert}->[2] + $thing->{sizes}->[2] / 2 );
            draw_prism($thing);
            glPopMatrix();
        }
        elsif ($thing->{type} == $OT_RAMP || $thing->{type} == $OT_STAIRS) {
            glPushMatrix();
            glTranslatef( 
                $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,  
                $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,  
                $thing->{vert}->[2] + $thing->{sizes}->[2] / 2 );
            draw_ramp($thing);
            glPopMatrix();
        }

    }

    glXSwapBuffers;
    $pending = 0;
}

################################################################################

sub queue_gl_draw
{
	return unless $gl_frame;
	return if $pending++;
	$main_win->DoWhenIdle(\&gl_draw);
}

sub gl_frame_vis_change
{
	return unless $gl_frame;
	queue_gl_draw;
}

sub view_frame_exposure_change
{
	return unless $gl_frame;
	queue_gl_draw;
}

sub gl_frame_config_change
{
    return unless $gl_frame;
    recalc_views();
    my $par = shift;
    my $w = $par->Width;
    my $h = $par->Height;
    glpMoveResizeWindow(0,0,$w,$h);
    glViewport(0,0,$w,$h);

    queue_gl_draw();
}

sub
gl_zoom_handler
{
    my ($frame, $zoom_direction) = @_;
    $log->debug("zoom handler $zoom_direction");

    $zoom_direction *= 4 if $control_key_down;

    # find vector from eye to target and reduce it by some factor, then add it to the eye vector
    $gl_var->{eye}->{x} += ($gl_var->{eye}->{x} - $gl_var->{target}->{x}) / 5 * $zoom_direction,
    $gl_var->{eye}->{y} += ($gl_var->{eye}->{y} - $gl_var->{target}->{y}) / 5 * $zoom_direction,
    $gl_var->{eye}->{z} += ($gl_var->{eye}->{z} - $gl_var->{target}->{z}) / 5 * $zoom_direction,

    queue_gl_draw();

}

sub main() 
{
	$SIG{INT} = sub { $log->debug("Ouch!\n") and exit; };

    # list of options
    my @options = qw(
        man
        help
        usage
        debug
        file=s
        new
    );

    GetOptions( \%options, @options ) or pod2usage(2);
    pod2usage(2) if $options{usage};
    pod2usage(1) if $options{help};
    pod2usage( -exitstatus => 0, -verbose => 2 ) if $options{man};

    $debug = $options{debug} ? 1 : 0;

    # put this in %options
    $options{bin_dir} = $Bin;

    if ($debug) {
        $ENV{log_appenders} = "file, screen";
        $ENV{log_level}     = "DEBUG";
    }
    else {
        $ENV{log_appenders} = "file";
        $ENV{log_level}     = "DEBUG";
    }
    $ENV{log_dir}       ||= $options{bin_dir};
    $ENV{log_file_name} ||= 'build';
    Log::Log4perl->init( $options{bin_dir} . '/log4perl.conf' );
    $log = get_logger();

    $log->debug("Running $0");

	init_tk;

    $gl_frame->bind("<Configure>",\&gl_frame_config_change);
    $gl_frame->bind("<Visibility>",\&gl_frame_vis_change);
    $gl_frame->bind("<ButtonPress-4>", [ \&gl_zoom_handler, -1 ]);
    $gl_frame->bind("<ButtonPress-5>", [ \&gl_zoom_handler, 1 ]);
    $view_frame->bind("<Expose>",\&view_frame_exposure_change);

#    $log->debug("gl frame coords " . $gl_frame->width() . ", " . $gl_frame->height());

    print "GLX_DOUBLEBUFFER = ", GLX_DOUBLEBUFFER, "\n";
    print "GLX_RGBA = ", GLX_RGBA, "\n";
    print "GLX_DEPTH_SIZE = ", GLX_DEPTH_SIZE, "\n";

    glpOpenWindow(
        parent=> hex($gl_frame->id), 
        width => $gl_frame->width(), 
        height => $gl_frame->height(),
        attributes => [ GLX_DOUBLEBUFFER, GLX_RGBA, GLX_DEPTH_SIZE, 16 ]);
    glClearColor(
          '0.475989929045548',
          '1',
          '0.940260929274433',
          1);

#    GLfloat light_ambient[] = { 0.0, 0.0, 0.0, 1.0 };
#    GLfloat light_diffuse[] = { 1.0, 1.0, 1.0, 1.0 };
#    GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };
#    GLfloat light_position[] = { 1.0, 1.0, 1.0, 0.0 };

    glLightfv (GL_LIGHT0, GL_AMBIENT, pack 'f4', 0.3, 0.3, 0.3, 1.0);
    glLightfv (GL_LIGHT0, GL_DIFFUSE, pack 'f4', 1.0, 1.0, 1.0, 1.0);
    glLightfv (GL_LIGHT0, GL_SPECULAR, pack 'f4', 1.0, 1.0, 1.0, 1.0);

    glEnable (GL_LIGHTING);
    glEnable (GL_LIGHT0);
    glEnable(GL_NORMALIZE);
    glDepthFunc(GL_LESS);

    glEnable(GL_DEPTH_TEST);

    glShadeModel(GL_FLAT);

    queue_gl_draw;

    push @system_things,

        {
            name => 'x y line',
            type => $OT_LINE,
            color => [ 1, 0, 0 ],
            verts => [
                [ -20, 0, 0 ],
                [ 0, 20, 0 ],
            ],
        },

        {
            name => 'x axis',
            type => $OT_LINE,
            color => [ 1, 0, 0 ],
            verts => [
                [ -20, 0, 0 ],
                [ 20, 0, 0 ],
            ],
        },

        {
            name => 'y axis',
            type => $OT_LINE,
            color => [ 0, 1, 0 ],
            verts => [
                [ 0, -20, 0 ],
                [ 0, 20, 0 ],
            ],
        },

        {
            name => 'z axis',
            type => $OT_LINE,
            color => [ 0, 0, 1 ],
            verts => [
                [ 0, 0, -20, ],
                [ 0, 0, 20, ],
            ],
        },

#        {
#            name => 'test',
#            type => $OT_LINE,
#            color => [ 1, 1, 1 ],
#            verts => [
#                [ 0, 0, 0, ],
#                [ 10, 20, 40, ],
#            ],
#        },

#        {
#            name => 'xz_box',
#            type => $OT_QUAD,
#            color => [ 1, 0, 1 ],
#            verts => [
#                [ 0, 5, 0, ],
#                [ 20, 5, 0, ],
#                [ 20, 5, 10, ],
#                [ 0, 5, 10, ],
#            ],
#        },

#        {
#            name => 'box',
#            type => $OT_CUBOID,
#            color => [ 0.5, 0.5, 0.5 ],
#            vert => [ 0, 5, 0, ],
#            sizes => [ 5, 10, 20 ],
#        },
#
#        {
#            name => 'box',
#            type => $OT_CUBOID,
#            color => [ 0.5, 0.5, 0.5 ],
#            vert => [ 10, 5, 0, ],
#            sizes => [ 5, 10, 20 ],
#        },

        {
            name => 'test',
            type => $OT_LINE,
            color => [ 1, 1, 1 ],
            verts => [
                [ -5, 0, 0, ],
                [ 0, 0, 0, ],
            ],
        },

        {
            name => 'test',
            type => $OT_LINE,
            color => [ 1, 1, 1 ],
            verts => [
                [ -5, 1, 0, ],
                [ 0, 1, 0, ],
            ],
        },

        {
            name => 'test',
            type => $OT_LINE,
            color => [ 1, 1, 1 ],
            verts => [
                [ -5, 2, 0, ],
                [ 0, 2, 0, ],
            ],
        },

        {
            name => 'test',
            type => $OT_LINE,
            color => [ 1, 1, 1 ],
            verts => [
                [ -5, 3, 0, ],
                [ 0, 3, 0, ],
            ],
        },

        {
            name => 'test',
            type => $OT_LINE,
            color => [ 1, 1, 1 ],
            verts => [
                [ -5, 4, 0, ],
                [ 0, 4, 0, ],
            ],
        },

        {
            name => 'test',
            type => $OT_LINE,
            color => [ 1, 1, 1 ],
            verts => [
                [ -5, 5, 0, ],
                [ 0, 5, 0, ],
            ],
        },

        ;

    recalc_views();
    refresh_views();

	open_from_file($options{file}) if $options{file};

	MainLoop;

	$log->debug("Byebye\n");

}

main;

__END__

=head

Pod for POV

Usage:	pov.pl [-help] [-man] [-usage] [-debug] [ -file <name> ] [ -new ]

Design notes, features to be added:
use digit strings as modifiers
use @digit strings as absolute positions
add xyz commands (only useful with digit strings)
rule-based series

=cut
