#ifndef _ZBAR_PROCESSOR_H_
#define _ZBAR_PROCESSOR_H_

#ifndef _ZBAR_H_
# error "include zbar.h in your application, **not** zbar/Processor.h"
#endif

#include "Exception.h"
#include "Image.h"

namespace zbar {
    
class Processor {
 public:
    static const int FOREVER = -1;

    Processor (bool threaded = true,
               const char *video_device = "",
               bool enable_display = true) {
        _processor = zbar_processor_create(threaded);
        if(!_processor)
            throw std::bad_alloc();
        init(video_device, enable_display);
    }

    ~Processor () {
        zbar_processor_destroy(_processor);
    }

    operator zbar_processor_t* () {
        return(_processor);
    }

    void init (const char *video_device = "",
               bool enable_display = true) {
        if(zbar_processor_init(_processor, video_device, enable_display))
            throw_exception(_processor);
    }

    void set_handler (Image::Handler& handler) {
        zbar_processor_set_data_handler(_processor, handler, &handler);
    }

    int set_config (zbar_symbol_type_t symbology,
                    zbar_config_t config,
                    int value) {
        return(zbar_processor_set_config(_processor, symbology,
                                          config, value));
    }

    int set_config (std::string cfgstr) {
        return(zbar_processor_parse_config(_processor, cfgstr.c_str()));
    }

    bool is_visible () {
        int rc = zbar_processor_is_visible(_processor);
        if(rc < 0)
            throw_exception(_processor);
        return(rc != 0);
    }

    void set_visible (bool visible = true) {
        if(zbar_processor_set_visible(_processor, visible) < 0)
            throw_exception(_processor);
    }

    void set_active (bool active = true) {
        if(zbar_processor_set_active(_processor, active) < 0)
            throw_exception(_processor);
    }

    const SymbolSet get_results () const {
        return(SymbolSet(zbar_processor_get_results(_processor)));
    }

    int user_wait (int timeout = FOREVER) {
        int rc = zbar_processor_user_wait(_processor, timeout);
        if(rc < 0)
            throw_exception(_processor);
        return(rc);
    }

    void process_one (int timeout = FOREVER) {
        if(zbar_process_one(_processor, timeout) < 0)
            throw_exception(_processor);
    }

    void process_image (Image& image) {
        if(zbar_process_image(_processor, image) < 0)
            throw_exception(_processor);
    }

    Processor& operator<< (Image& image) {
        process_image(image);
        return(*this);
    }

    void force_format (unsigned long input_format,
                       unsigned long output_format) {
        if(zbar_processor_force_format(_processor, input_format,
                                        output_format))
            throw_exception(_processor);
    }

    void force_format (std::string& input_format,
                       std::string& output_format) {
        unsigned long ifourcc = zbar_fourcc_parse(input_format.c_str());
        unsigned long ofourcc = zbar_fourcc_parse(output_format.c_str());
        if(zbar_processor_force_format(_processor, ifourcc, ofourcc))
            throw_exception(_processor);
    }

    void request_size (int width, int height) {
        zbar_processor_request_size(_processor, width, height);
    }

    void request_interface (int version) {
        zbar_processor_request_interface(_processor, version);
    }

    void request_iomode (int iomode) {
        if(zbar_processor_request_iomode(_processor, iomode))
            throw_exception(_processor);
    }

 private:
    zbar_processor_t *_processor;
};

}

#endif
