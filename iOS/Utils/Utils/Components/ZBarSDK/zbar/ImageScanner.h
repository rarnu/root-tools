#ifndef _ZBAR_IMAGE_SCANNER_H_
#define _ZBAR_IMAGE_SCANNER_H_

#ifndef _ZBAR_H_
# error "include zbar.h in your application, **not** zbar/ImageScanner.h"
#endif

#include "Image.h"

namespace zbar {

class ImageScanner {
public:
    ImageScanner (zbar_image_scanner_t *scanner = NULL) {
        if(scanner)
            _scanner = scanner;
        else
            _scanner = zbar_image_scanner_create();
    }

    ~ImageScanner () {
        zbar_image_scanner_destroy(_scanner);
    }

    operator zbar_image_scanner_t* () const {
        return(_scanner);
    }

    void set_handler (Image::Handler &handler) {
        zbar_image_scanner_set_data_handler(_scanner, handler, &handler);
    }

    int set_config (zbar_symbol_type_t symbology,
                    zbar_config_t config,
                    int value) {
        return(zbar_image_scanner_set_config(_scanner, symbology,
                                              config, value));
    }

    int set_config (std::string cfgstr) {
        return(zbar_image_scanner_parse_config(_scanner, cfgstr.c_str()));
    }

    void enable_cache (bool enable = true) {
        zbar_image_scanner_enable_cache(_scanner, enable);
    }

    void recycle_image (Image &image) {
        zbar_image_scanner_recycle_image(_scanner, image);
    }

    const SymbolSet get_results () const {
        return(SymbolSet(zbar_image_scanner_get_results(_scanner)));
    }

    int scan (Image& image) {
        return(zbar_scan_image(_scanner, image));
    }

    ImageScanner& operator<< (Image& image) {
        scan(image);
        return(*this);
    }

private:
    zbar_image_scanner_t *_scanner;
};

}

#endif
