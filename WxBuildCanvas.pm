package WxBuildCanvas;

use strict;

use Log::Log4perl qw(get_logger);
my $log = get_logger;

use Wx::Event qw(EVT_PAINT EVT_SIZE EVT_ERASE_BACKGROUND EVT_IDLE EVT_TIMER EVT_MOUSE_EVENTS);
# must load OpenGL *before* Wx::GLCanvas
use OpenGL qw(:glconstants :glfunctions);
use OpenGL;
use OpenGL::Simple::GLUT qw(:all);
use Data::Dumper;
use Time::Piece;
use Wx::GLCanvas;
use Wx qw(wxTheApp :cursor :bitmap);

use base qw(Wx::GLCanvas Class::Accessor::Fast);

__PACKAGE__->mk_accessors( qw(timer x_rot y_rot dirty resize_pending init ortho model width height) );

sub new {
    my( $class, $parent ) = @_;
    my $self = $class->SUPER::new( $parent);
#    my $self = $class->SUPER::new( $parent,-1, [-1, -1], [-1, -1], 0, "GLCanvas", [ WX_GL_DOUBLEBUFFER, WX_GL_RGBA, WX_GL_DEPTH_SIZE, 16 ] );

    my $timer = $self->timer( Wx::Timer->new( $self ) );
#    $timer->Start( 1000 );

    $self->x_rot( 0 );
    $self->y_rot( 0 );
    $self->ortho( 0 );
    $self->resize_pending( 0 );
    $self->dirty( 0 );

    EVT_PAINT( $self,
                sub {
#                    $log->debug("paint");
                    my $dc = Wx::PaintDC->new( $self );
                    $self->Render( $dc );

                } );
    EVT_SIZE( $self,
                sub {
#                    $log->debug("size");
                    $self->resize_pending( 1 );
                    $self->Resize( $self->GetSizeWH );
                } );
    EVT_IDLE( $self,
                sub {
#                    $log->debug("idle : " . $self->dirty);
                    return unless $self->dirty || $self->resize_pending;
                    $self->Refresh;
                } );
    EVT_TIMER( $self, -1, sub {
                   my( $self, $e ) = @_;

#                   $self->x_rot( $self->x_rot - 1 );
#                   $self->y_rot( $self->y_rot + 2 );

                   $self->dirty( 1 );
                   Wx::WakeUpIdle;
               } );

#    Wx::Event::EVT_MOUSE_EVENTS( $self,
#        sub {
#            my ($self, $event) = @_;
#            $log->debug("mouse event :" . $event->ControlDown);
#        } );
    Wx::Event::EVT_MOUSEWHEEL( $self, \&WxBuildView::zoom_tracker );


    return $self;
}

sub view {
    my ($self, $view) = @_;

    # need a custom accessor to do the setup stuff that needs to know about the stuff
    if ($view) {
        $self->{view} = $view;
        $log->debug("set view for canvas " . Dumper($view));

        Wx::Event::EVT_LEFT_DCLICK( $self, sub {
            my ($self, $event) = @_;
            $view->toggle_controls;
        });

        if ($view->ortho) {

            Wx::Event::EVT_BUTTON( $view->panel, $view->panel->{eye_to_origin_btn},
                sub { 
                    my ($panel, $event) = @_;
                    $panel->{canvas}->view->reset_eye;
                }
            );

            Wx::Event::EVT_BUTTON( $view->panel, $view->panel->{axes_to_origin_btn},
                sub { 
                    my ($panel, $event) = @_;
                    $panel->{canvas}->view->reset_view;
                }
            );

            Wx::Event::EVT_MOTION( $self, sub {
                my ($self, $event) = @_;
                if ($event->LeftIsDown) {
                    $view->drag_tracker($event->GetX, $event->GetY );
                }
                else {
                    $view->move_tracker($event->GetX, $event->GetY, {} );
                }
            });

            Wx::Event::EVT_LEFT_DOWN( $self, sub {
                my ($self, $event) = @_;
                $view->click_tracker( 0, $event->GetX, $event->GetY );
            });

            Wx::Event::EVT_LEFT_UP( $self, sub {
                my ($self, $event) = @_;
                $view->click_tracker( 1, $event->GetX, $event->GetY );
            });

        }
    }

    return $self->{view};
}

sub GetContext {
    my( $self ) = @_;

    if( Wx::wxVERSION >= 2.009 ) {
        return $self->{context} ||= Wx::GLContext->new( $self );
    } else {
        return $self->SUPER::GetContext;
    }
}

sub SetCurrent {
    my( $self, $context ) = @_;

    if( Wx::wxVERSION >= 2.009 ) {
        return $self->SUPER::SetCurrent( $context );
    } else {
        return $self->SUPER::SetCurrent;
    }
}

sub Resize {
    my( $self, $width, $height ) = @_;

    $log->debug("try resize with $width, $height");
    return unless $self->GetContext;
    return unless defined $width;
    $log->debug("Resize $width, $height");

    $self->SetCurrent( $self->GetContext );
    glViewport( 0, 0, $width, $height );

    $self->width($width);
    $self->height($height);

    if ($self->ortho) {
        return unless my $view = $self->view;
        $self->view->recalc_view;
    }

    $self->resize_pending(0);

}

use Math::Trig;

sub DESTROY {
    my( $self ) = @_;

    if ($self->timer) {
        $self->timer->Stop;
        $self->timer( undef );
    }
}

sub cube {
    my( @v ) = ( [ 1, 1, 1 ], [ -1, 1, 1 ],
                 [ -1, -1, 1 ], [ 1, -1, 1 ],
                 [ 1, 1, -1 ], [ -1, 1, -1 ],
                 [ -1, -1, -1 ], [ 1, -1, -1 ] );
    my( @c ) = ( [ 1, 1, 0 ], [ 1, 0, 1 ],
                 [ 0, 1, 1 ], [ 1, 1, 1 ],
                 [ 0, 0, 1 ], [ 0, 1, 0 ],
                 [ 1, 0, 1 ], [ 1, 1, 0 ] );
    my( @s ) = ( [ 0, 1, 2, 3 ], [ 4, 5, 6, 7 ],
                 [ 0, 1, 5, 4 ], [ 2, 3, 7, 6 ],
                 [ 1, 2, 6, 5 ], [ 0, 3, 7, 4 ] );

    for my $i ( 0 .. 5 ) {
        my $s = $s[$i];
        glBegin(GL_QUADS);
        foreach my $j ( @$s ) {
            glColor3f( @{$c[$j]} );
            glVertex3f( @{$v[$j]} );
        }
        glEnd();
    }
}

sub InitGL {
    my $self = shift;

#    return if $self->init;
#    return unless $self->GetContext;
#    $self->init( 1 );
    $log->debug("InitGL");

    $self->SetCurrent( $self->GetContext );
    glClearColor(
        '0.475989929045548',
        '1',
        '0.940260929274433',
        1);

    glEnable(GL_NORMALIZE);

    if ($self->ortho) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    }
    else {
        glLightfv (GL_LIGHT0, GL_AMBIENT, pack 'f4', 0.3, 0.3, 0.3, 1.0);
        glLightfv (GL_LIGHT0, GL_DIFFUSE, pack 'f4', 1.0, 1.0, 1.0, 1.0);
        glLightfv (GL_LIGHT0, GL_SPECULAR, pack 'f4', 1.0, 1.0, 1.0, 1.0);

        glEnable (GL_LIGHTING);
        glEnable (GL_LIGHT0);
        
        glShadeModel(GL_FLAT);

        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

        glDepthFunc(GL_LESS);

        glEnable(GL_DEPTH_TEST);
    }

}

sub Render {
    my( $self, $dc ) = @_;

    return unless my $view = $self->view;
    return unless $self->GetContext;

    $self->Resize($self->GetSizeWH) if $self->resize_pending;

    $self->SetCurrent( $self->GetContext );

#    unless ($self->ortho) {
#        glTranslatef( 0, 0, -5 );
#    }
#    glRotatef( $self->x_rot, 1, 0, 0 );
#    glRotatef( $self->y_rot, 0, 0, 1 );
#
#    cube();

    $self->gl_draw();

    glFlush();

    $self->SwapBuffers();

    $self->dirty(0);

    if (! $self->ortho && (my $capture_start = wxTheApp->{app}->capturing)) {
        my $capture_index = wxTheApp->{app}->capture_index;
        wxTheApp->{app}->capture_index($capture_index+1);

        # don't want to use sprintf for leading 0s, too slow
        $capture_index = "_" . ("0" x (6 - length($capture_index))) . $capture_index;
        
        my $filename = "capture/" . $view->name . "_${capture_start}$capture_index.jpg";
        unless (-f $filename) {
            $log->debug("capture to $filename");
            my ($width, $height) = $self->GetSizeWH;
            my $bitmap = Wx::Bitmap->new($width,$height);
            my $memDC = Wx::MemoryDC->new();
            $memDC->SelectObject($bitmap);
            $memDC->Blit(0,0,$width,$height,$dc,0,0);
            $bitmap->SaveFile($filename, wxBITMAP_TYPE_JPEG);
        }
    }
}

################################################################################

sub gl_draw {
    my ($self) = @_;

    my $view = $self->view
        or return;

    my $model = $self->model
        or return;

#    $log->debug("draw view " . $view->name);

#    refresh_views();

    if ($view->up_vector_above_eye) {
        if ($self->ortho) {
            $view->up->x($view->eye->x - $view->target->x);
            $view->up->y($view->eye->y - $view->target->y);
            $view->up->z($view->eye->z - $view->target->z);

            $view->up->dim($view->y_dim, $view->eye->dim($view->y_dim) - 10 * $view->y_axis_direction);
            $view->debug("up = " . $view->up->dump);
        }
        else {
            $view->up->x($view->eye->x - $view->target->x);
            $view->up->y($view->eye->y + 10);
            $view->up->z($view->eye->z - $view->target->z);
        }
    }

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    my ($width, $height) = ($self->width, $self->height);
    return unless $width;
    if ($self->ortho) {
        my $n = $view->size / 2;
        my $y_centre = ($view->y_axis_direction * $view->y_centre * -1);
        if ($width <= $height) {
            # left, right, bottom, top, near, far
            glOrtho(
                $view->x_centre - $n, $view->x_centre + $n,
                $y_centre - $n*$height/$width, $y_centre + $n*$height/$width, 
                1000, -1000);
        }
        else {
            glOrtho(
                $view->x_centre - $n/($height/$width), $view->x_centre + $n/($height/$width), 
                $y_centre - $n, $y_centre + $n,
                -1000, 1000);
        }

    }
    else {

        glLightfv (GL_LIGHT0, GL_AMBIENT, pack 'f4', 0.3, 0.3, 0.3, 1.0);
        glLightfv (GL_LIGHT0, GL_DIFFUSE, pack 'f4', 1.0, 1.0, 1.0, 1.0);
        glLightfv (GL_LIGHT0, GL_SPECULAR, pack 'f4', 1.0, 1.0, 1.0, 1.0);

        glEnable (GL_LIGHTING);
        glEnable (GL_LIGHT0);
        
        glShadeModel(GL_FLAT);

        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

        glDepthFunc(GL_LESS);

        glEnable(GL_DEPTH_TEST);

        gluPerspective(
            $view->perspective->{angle}, 
            $width / $height,
            $view->perspective->{near},
            $view->perspective->{far});

        glLightfv (GL_LIGHT0, GL_POSITION, pack 'f4', $view->light->vertex, 0.0 );

    }

    glMatrixMode(GL_MODELVIEW);

    glLoadIdentity();   #  clear the matrix

    #  viewing transformation
    gluLookAt(
        $view->eye->vertex,
        $view->target->vertex,
        $view->up->vertex,
    );

    #  modeling transformation
#    glScalef($view->scale->vertex);

    glRotatef( $self->x_rot, 1, 0, 0 );
    glRotatef( $self->y_rot, 0, 0, 1 );

    glColor3f(1.0, 1.0, 0.0);

#    my @test = (
#        [ 10,10,0, ],
#        [ 10,20,0, ],
#        [ 13,19,0, ],
#        [ 15,18,0, ],
#        [ 17,16,0, ],
#        [ 18,14,0, ],
#        [ 19,11,0, ],
#    );
#
#    glBegin(GL_TRIANGLE_FAN);
#
#    # list of vertices; centre of end, first rim point, then around the rim and repeat the first point
#    for my $vertex (@test) {
#        glVertex3f( @{ $vertex } );
#    }
#
#    glEnd();

    my $app = wxTheApp->{app};

#    $log->debug($self->{view}->name . ": start rendering " . scalar (values %{ $app->system_thing }) . " system things and " . scalar @{ $model->things } . " model things");
    for my $thing (values %{ $app->system_thing }, @{ $model->things }) {

        next if $thing->{no_render};
        
        next if $thing->{skip_view}->{$view->name};

#        $log->debug("rendering $thing->{name}");
#        $log->debug(Dumper($thing)) if $thing->{debug};

        # we've already called refresh_views() and that updates the selected flags so we don't need to
        if ($view->render_by_selection) {
            if ($view->render_unselected) {
                next if $thing->{_selected};
            }
            else {
                next unless $thing->{_selected};
            }
        }

        if ($view->filter_shown_by_position) {
            next unless $view->thing_passes_filter($thing);
        }

        glColor3f(@{$thing->{color}});

        if ($thing->{type} == $WxBuild::OT_LINE) {
            glBegin(GL_LINES);
            glVertex3f(@{$thing->{verts}->[0]});
            glVertex3f(@{$thing->{verts}->[1]});
            glEnd();
        }
        elsif ($thing->{type} == $WxBuild::OT_QUAD) {
            glBegin(GL_QUADS);
            glVertex3f(@{$thing->{verts}->[0]});
            glVertex3f(@{$thing->{verts}->[1]});
            glVertex3f(@{$thing->{verts}->[2]});
            glVertex3f(@{$thing->{verts}->[3]});
            glEnd();
        }
        elsif ($thing->{type} == $WxBuild::OT_CUBOID) {
            glPushMatrix();
            glTranslatef( 
                $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,  
                $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,  
                $thing->{vert}->[2] + $thing->{sizes}->[2] / 2 );
            myBox(@{ $thing->{sizes} }, $thing->{solid});
            glPopMatrix();
        }
        elsif ($thing->{type} == $WxBuild::OT_PRISM) {
            glPushMatrix();
            glTranslatef( 
                $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,  
                $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,  
                $thing->{vert}->[2] + $thing->{sizes}->[2] / 2 );
            draw_prism($thing);
            glPopMatrix();
        }
        elsif ($thing->{type} == $WxBuild::OT_RAMP || $thing->{type} == $WxBuild::OT_STAIRS) {
            glPushMatrix();
            glTranslatef( 
                $thing->{vert}->[0] + $thing->{sizes}->[0] / 2,  
                $thing->{vert}->[1] + $thing->{sizes}->[1] / 2,  
                $thing->{vert}->[2] + $thing->{sizes}->[2] / 2 );
            draw_ramp($thing);
            glPopMatrix();
        }

    }

#    glXSwapBuffers;
#    $pending = 0;
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


1;

