#include <X11/Xlib.h>
#include <EGL/egl.h>

EGLint* getAttribs( void ) ; 

void c_routine(Display *x_dpy, EGLDisplay egl_dpy, EGLConfig egl_config) ; 

void c_draw(EGLDisplay egl_dpy, EGLSurface egl_surf) ;
