#ifndef _ZBAR_SCANNER_H_
#define _ZBAR_SCANNER_H_


#ifndef _ZBAR_H_
# error "include zbar.h in your application, **not** zbar/Scanner.h"
#endif

#include <stdio.h>

namespace zbar {

class Scanner {
 public:

    Scanner (Decoder& decoder) {
        _scanner = zbar_scanner_create(decoder._decoder);
    }

    Scanner (Decoder* decoder = NULL) {
        zbar_decoder_t *zdcode = NULL;
        if(decoder)
            zdcode = decoder->_decoder;
        _scanner = zbar_scanner_create(zdcode);
    }

    ~Scanner () {
        zbar_scanner_destroy(_scanner);
    }
    
    void reset () {
        zbar_scanner_reset(_scanner);
    }

    zbar_symbol_type_t new_scan () {
        _type = zbar_scanner_new_scan(_scanner);
        return(_type);
    }

    zbar_symbol_type_t flush (){
        _type = zbar_scanner_flush(_scanner);
        return(_type);
    }

    zbar_symbol_type_t scan_y (int y) {
        _type = zbar_scan_y(_scanner, y);
        return(_type);
    }

    Scanner& operator<< (int y){
        _type = zbar_scan_y(_scanner, y);
        return(*this);
    }

    zbar_symbol_type_t scan_rgb24 (unsigned char *rgb) {
        _type = zbar_scan_rgb24(_scanner, rgb);
        return(_type);
    }

    Scanner& operator<< (unsigned char *rgb) {
        _type = zbar_scan_rgb24(_scanner, rgb);
        return(*this);
    }

    unsigned get_width () const {
        return(zbar_scanner_get_width(_scanner));
    }

    zbar_color_t get_color () const {
        return(zbar_scanner_get_color(_scanner));
    }

    zbar_symbol_type_t get_type () const {
        return(_type);
    }

    operator zbar_scanner_t* () const {
        return(_scanner);
    }

    const zbar_scanner_t *get_c_scanner () const {
        return(_scanner);
    }

 private:
    zbar_scanner_t *_scanner;
    zbar_symbol_type_t _type;
};

}

#endif
