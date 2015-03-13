#ifndef _ZBAR_IMAGE_H_
#define _ZBAR_IMAGE_H_

#ifndef _ZBAR_H_
# error "include zbar.h in your application, **not** zbar/Image.h"
#endif

#include <assert.h>
#include <iterator>
#include "Symbol.h"
#include "Exception.h"

namespace zbar {

class Video;

class Image {
public:
    class Handler {
    public:
        virtual ~Handler() { }

        virtual void image_callback(Image &image) = 0;

        operator zbar_image_data_handler_t* () const  {
            return(_cb);
        }

    private:
        static void _cb (zbar_image_t *zimg, const void *userdata) {
            if(userdata) {
                Image *image = (Image*)zbar_image_get_userdata(zimg);
                if(image)
                    ((Handler*)userdata)->image_callback(*image);
                else {
                    Image tmp(zimg, 1);
                    ((Handler*)userdata)->image_callback(tmp);
                }
            }
        }
    };

    class SymbolIterator : public zbar::SymbolIterator {
    public:
        SymbolIterator () : zbar::SymbolIterator() {
        }

        SymbolIterator (const SymbolSet &syms) : zbar::SymbolIterator(syms) {
        }

        SymbolIterator (const SymbolIterator& iter) : zbar::SymbolIterator(iter) {
        }
    };

    Image (unsigned width = 0,
           unsigned height = 0,
           const std::string& format = "",
           const void *data = NULL,
           unsigned long length = 0)
        : _img(zbar_image_create()) {
        zbar_image_set_userdata(_img, this);
        if(width && height)
            set_size(width, height);
        if(format.length())
            set_format(format);
        if(data && length)
            set_data(data, length);
    }

    ~Image () {
        if(zbar_image_get_userdata(_img) == this)
            zbar_image_set_userdata(_img, NULL);
        zbar_image_ref(_img, -1);
    }

    operator const zbar_image_t* () const {
        return(_img);
    }

    operator zbar_image_t* () {
        return(_img);
    }

    unsigned long get_format () const {
        return(zbar_image_get_format(_img));
    }

    void set_format (unsigned long format){
        zbar_image_set_format(_img, format);
    }

    void set_format (const std::string& format) {
        unsigned long fourcc = zbar_fourcc_parse(format.c_str());
        zbar_image_set_format(_img, fourcc);
    }

    unsigned get_sequence () const{
        return(zbar_image_get_sequence(_img));
    }

    void set_sequence (unsigned sequence_num) {
        zbar_image_set_sequence(_img, sequence_num);
    }

    unsigned get_width () const {
        return(zbar_image_get_width(_img));
    }

    unsigned get_height () const {
        return(zbar_image_get_height(_img));
    }

    void get_size (unsigned &width, unsigned &height) const {
        zbar_image_get_size(_img, &width, &height);
    }

    void set_size (unsigned width, unsigned height) {
        zbar_image_set_size(_img, width, height);
    }

    void get_crop (unsigned &x,
                   unsigned &y,
                   unsigned &width,
                   unsigned &height) const {
        zbar_image_get_crop(_img, &x, &y, &width, &height);
    }


    void set_crop (unsigned x,
                   unsigned y,
                   unsigned width,
                   unsigned height) {
        zbar_image_set_crop(_img, x, y, width, height);
    }

    const void *get_data () const {
        return(zbar_image_get_data(_img));
    }

    unsigned long get_data_length () const {
        return(zbar_image_get_data_length(_img));
    }

    void set_data (const void *data,
                   unsigned long length) {
        zbar_image_set_data(_img, data, length, _cleanup);
    }

    Image convert (unsigned long format) const {
        zbar_image_t *img = zbar_image_convert(_img, format);
        if(img)
            return(Image(img));
        throw FormatError();
    }

    Image convert (std::string format) const {
        unsigned long fourcc = zbar_fourcc_parse(format.c_str());
        return(convert(fourcc));
    }

    Image convert (unsigned long format,
                   unsigned width,
                   unsigned height) const {
        zbar_image_t *img =
            zbar_image_convert_resize(_img, format, width, height);
        if(img)
            return(Image(img));
        throw FormatError();
    }

    const SymbolSet get_symbols () const {
        return(SymbolSet(zbar_image_get_symbols(_img)));
    }

    void set_symbols (const SymbolSet &syms) {
        zbar_image_set_symbols(_img, syms);
    }

    SymbolIterator symbol_begin () const {
        return(SymbolIterator(get_symbols()));
    }

    SymbolIterator symbol_end () const {
        return(SymbolIterator());
    }

protected:

    friend class Video;

    Image (zbar_image_t *src, int refs = 0) : _img(src) {
        if(refs)
            zbar_image_ref(_img, refs);
        zbar_image_set_userdata(_img, this);
    }

    static void _cleanup (zbar_image_t *img) {
        assert(img);
    }

private:
    zbar_image_t *_img;
};

}

#endif
