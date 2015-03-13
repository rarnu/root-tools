#ifndef _ZBAR_DECODER_H_
#define _ZBAR_DECODER_H_

#ifndef _ZBAR_H_
# error "include zbar.h in your application, **not** zbar/Decoder.h"
#endif

#include <string>

namespace zbar {

class Decoder {
 public:
    class Handler {
    public:
        virtual ~Handler() { }
        virtual void decode_callback(Decoder &decoder) = 0;
    };
    Decoder () : _handler(NULL) {
        _decoder = zbar_decoder_create();
    }

    ~Decoder () {
        zbar_decoder_destroy(_decoder);
    }

    void reset () {
        zbar_decoder_reset(_decoder);
    }

    void new_scan () {
        zbar_decoder_new_scan(_decoder);
    }

    zbar_symbol_type_t decode_width (unsigned width)  {
        return(zbar_decode_width(_decoder, width));
    }

    Decoder& operator<< (unsigned width) {
        zbar_decode_width(_decoder, width);
        return(*this);
    }

    zbar_color_t get_color () const {
        return(zbar_decoder_get_color(_decoder));
    }

    zbar_symbol_type_t get_type () const {
        return(zbar_decoder_get_type(_decoder));
    }

    const char *get_symbol_name () const {
        return(zbar_get_symbol_name(zbar_decoder_get_type(_decoder)));
    }

    const char *get_addon_name () const {
        return(zbar_get_addon_name(zbar_decoder_get_type(_decoder)));
    }

    const char *get_data_chars() const {
        return(zbar_decoder_get_data(_decoder));
    }

    const std::string get_data_string() const {
        return(std::string(zbar_decoder_get_data(_decoder),
                           zbar_decoder_get_data_length(_decoder)));
    }

    const std::string get_data() const {
        return(get_data_string());
    }

    int get_data_length() const {
        return(zbar_decoder_get_data_length(_decoder));
    }

    int get_direction() const {
        return(zbar_decoder_get_direction(_decoder));
    }

    void set_handler (Handler &handler) {
        _handler = &handler;
        zbar_decoder_set_handler(_decoder, _cb);
        zbar_decoder_set_userdata(_decoder, this);
    }

    int set_config (zbar_symbol_type_t symbology, zbar_config_t config, int value) {
        return(zbar_decoder_set_config(_decoder, symbology, config, value));
    }

    int set_config (std::string cfgstr) {
        return(zbar_decoder_parse_config(_decoder, cfgstr.c_str()));
    }

 private:
    friend class Scanner;
    zbar_decoder_t *_decoder;
    Handler *_handler;

    static void _cb (zbar_decoder_t *cdcode) {
        Decoder *dcode = (Decoder*)zbar_decoder_get_userdata(cdcode);
        if(dcode && dcode->_handler)
            dcode->_handler->decode_callback(*dcode);
    }
};

}

#endif
