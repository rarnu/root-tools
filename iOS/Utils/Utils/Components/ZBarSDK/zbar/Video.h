#ifndef _ZBAR_VIDEO_H_
#define _ZBAR_VIDEO_H_

#ifndef _ZBAR_H_
# error "include zbar.h in your application, **not** zbar/Video.h"
#endif

#include "Image.h"

namespace zbar {

class Video {
public:
    Video (zbar_video_t *video = NULL) {
        if(video)
            _video = video;
        else
            _video = zbar_video_create();
    }

    Video (std::string& device) {
        _video = zbar_video_create();
        open(device);
    }

    ~Video () {
        zbar_video_destroy(_video);
    }

    operator zbar_video_t* () const {
        return(_video);
    }

    void open (std::string& device) {
        if(zbar_video_open(_video, device.c_str()))
            throw_exception(_video);
    }

    void close () {
        if(zbar_video_open(_video, NULL))
            throw_exception(_video);
    }

    void init (unsigned long fourcc) {
        if(zbar_video_init(_video, fourcc))
            throw_exception(_video);
    }

    void init (std::string& format) {
        unsigned int fourcc = (unsigned int)zbar_fourcc_parse(format.c_str());
        if(zbar_video_init(_video, fourcc))
            throw_exception(_video);
    }

    int get_fd () {
        return(zbar_video_get_fd(_video));
    }

    int get_width () {
        return(zbar_video_get_width(_video));
    }

    int get_height () {
        return(zbar_video_get_height(_video));
    }

    void enable (bool enable = true) {
        if(zbar_video_enable(_video, enable))
            throw_exception(_video);
    }

    Image next_image () {
        zbar_image_t *img = zbar_video_next_image(_video);
        if(!img)
            throw_exception(_video);
        return(Image(img));
    }

    void request_size (int width, int height) {
        zbar_video_request_size(_video, width, height);
    }

    void request_interface (int version) {
        zbar_video_request_interface(_video, version);
    }

    void request_iomode (int iomode) {
        if(zbar_video_request_iomode(_video, iomode))
            throw_exception(_video);
    }

private:
    zbar_video_t *_video;
};

}

#endif
