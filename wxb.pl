#!/usr/bin/perl -w -- 

use strict;
use Log::Log4perl qw(get_logger);
use Getopt::Long;
use FindBin qw($Bin);
use OpenGL;
use Pod::Usage;

package BuildApp;

use Wx qw[:everything];
use base qw(Wx::App);

use WxBuild;
use WxBuildWin;

sub new {
	my( $class, @args ) = @_;
	my $self = $class->SUPER::new( @args );

    return $self;
}

sub OnInit {
	my( $self ) = shift;

	Wx::InitAllImageHandlers();

    # create the main window used for the application screens
	$self->{mw} = WxBuildWin->new();

    # fix things we can't do in WxGlade
    my @styles = map { wxSB_NORMAL } (1 .. $self->{mw}->{frame_1_statusbar}->GetFieldsCount);
    $styles[1] = wxSB_FLAT;
	$self->{mw}->{frame_1_statusbar}->SetStatusStyles(@styles);

	$self->SetTopWindow($self->{mw});
	$self->{mw}->Show(1);

    # create application controller
    $self->{app} = WxBuild->new(frame => $self->{mw}, model_file => 'temple.yml');

	return 1;
}

package main;

my $log = get_logger;
my %options;
my $debug;

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
    $ENV{log_level}     = "INFO";
}
$ENV{log_dir}       ||= $options{bin_dir};
$ENV{log_file_name} ||= 'wxb';
Log::Log4perl->init( $options{bin_dir} . '/log4perl.conf' );
$log = get_logger();

$log->debug("Running $0");

my $app =BuildApp->new();

$app->MainLoop();
