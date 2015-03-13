#ifndef _ZBAR_WINDOW_H_
#define _ZBAR_WINDOW_H_

#ifndef _ZBAR_H_
# error "include zbar.h in your application, **not** zbar/Window.h"
#endif

#include "Image.h"

namespace zbar {

class Window {
public:
    Window (zbar_window_t *window = NULL) {
        if(window)
            _window = window;
        else
            _window = zbar_window_create();
    }

    Window (void *x11_display_w32_hwnd,
            unsigned long x11_drawable) {
        _window = zbar_window_create();
        attach(x11_display_w32_hwnd, x11_drawable);
    }

    ~Window () {
        zbar_window_destroy(_window);
    }

    operator zbar_window_t* () const {
        return(_window);
    }

    void attach (void *x11_display_w32_hwnd,
                 unsigned long x11_drawable = 0) {
        if(zbar_window_attach(_window,
                               x11_display_w32_hwnd, x11_drawable) < 0)
            throw_exception(_window);
    }
    
    void set_overlay (int level) {
        zbar_window_set_overlay(_window, level);
    }

    void draw (Image& image) {
        if(zbar_window_draw(_window, image) < 0)
            throw_exception(_window);
    }

    void clear () {
        if(zbar_window_draw(_window, NULL) < 0)
            throw_exception(_window);
    }

    void redraw () {
        if(zbar_window_redraw(_window) < 0)
            throw_exception(_window);
    }

    void resize (unsigned width, unsigned height) {
        if(zbar_window_resize(_window, width, height) < 0)
            throw_exception(_window);
    }

private:
    zbar_window_t *_window;
};

static inline void negotiate_format (Video& video, Window& window) {
    if(zbar_negotiate_format(video, window) < 0)
        throw_exception(video);
}

}

#endif
