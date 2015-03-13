#ifndef _ZBAR_H_
#define _ZBAR_H_
#ifdef __cplusplus

namespace zbar {
    extern "C" {
#endif

typedef enum zbar_color_e {
    ZBAR_SPACE = 0,
    ZBAR_BAR = 1,
} zbar_color_t;

typedef enum zbar_symbol_type_e {
    ZBAR_NONE        =      0,
    ZBAR_PARTIAL     =      1,
    ZBAR_EAN2        =      2,
    ZBAR_EAN5        =      5,
    ZBAR_EAN8        =      8,
    ZBAR_UPCE        =      9,
    ZBAR_ISBN10      =     10,
    ZBAR_UPCA        =     12,
    ZBAR_EAN13       =     13,
    ZBAR_ISBN13      =     14,
    ZBAR_COMPOSITE   =     15,
    ZBAR_I25         =     25,
    ZBAR_DATABAR     =     34,
    ZBAR_DATABAR_EXP =     35,
    ZBAR_CODABAR     =     38,
    ZBAR_CODE39      =     39,
    ZBAR_PDF417      =     57,
    ZBAR_QRCODE      =     64,
    ZBAR_CODE93      =     93,
    ZBAR_CODE128     =    128,
    ZBAR_SYMBOL      = 0x00ff,
    ZBAR_ADDON2      = 0x0200,
    ZBAR_ADDON5      = 0x0500,
    ZBAR_ADDON       = 0x0700,
} zbar_symbol_type_t;

typedef enum zbar_orientation_e {
    ZBAR_ORIENT_UNKNOWN = -1,
    ZBAR_ORIENT_UP,
    ZBAR_ORIENT_RIGHT,
    ZBAR_ORIENT_DOWN,
    ZBAR_ORIENT_LEFT,
} zbar_orientation_t;

typedef enum zbar_error_e {
    ZBAR_OK = 0,
    ZBAR_ERR_NOMEM,
    ZBAR_ERR_INTERNAL,
    ZBAR_ERR_UNSUPPORTED,
    ZBAR_ERR_INVALID,
    ZBAR_ERR_SYSTEM,
    ZBAR_ERR_LOCKING,
    ZBAR_ERR_BUSY,
    ZBAR_ERR_XDISPLAY,
    ZBAR_ERR_XPROTO,
    ZBAR_ERR_CLOSED,
    ZBAR_ERR_WINAPI,
    ZBAR_ERR_NUM
} zbar_error_t;

typedef enum zbar_config_e {
    ZBAR_CFG_ENABLE = 0,
    ZBAR_CFG_ADD_CHECK,
    ZBAR_CFG_EMIT_CHECK,
    ZBAR_CFG_ASCII,
    ZBAR_CFG_NUM,
    ZBAR_CFG_MIN_LEN = 0x20,
    ZBAR_CFG_MAX_LEN,
    ZBAR_CFG_UNCERTAINTY = 0x40,
    ZBAR_CFG_POSITION = 0x80,
    ZBAR_CFG_X_DENSITY = 0x100,
    ZBAR_CFG_Y_DENSITY,
} zbar_config_t;

typedef enum zbar_modifier_e {
    ZBAR_MOD_GS1 = 0,
    ZBAR_MOD_AIM,
    ZBAR_MOD_NUM,
} zbar_modifier_t;

extern int zbar_version(unsigned *major, unsigned *minor);
extern void zbar_set_verbosity(int verbosity);

extern void zbar_increase_verbosity(void);

extern const char *zbar_get_symbol_name(zbar_symbol_type_t sym);

extern const char *zbar_get_addon_name(zbar_symbol_type_t sym);

extern const char *zbar_get_config_name(zbar_config_t config);
extern const char *zbar_get_modifier_name(zbar_modifier_t modifier);

extern const char *zbar_get_orientation_name(zbar_orientation_t orientation);

extern int zbar_parse_config(const char *config_string, zbar_symbol_type_t *symbology, zbar_config_t *config, int *value);

#define zbar_fourcc(a, b, c, d)                 \
        ((unsigned long)(a) |                   \
         ((unsigned long)(b) << 8) |            \
         ((unsigned long)(c) << 16) |           \
         ((unsigned long)(d) << 24))

static inline unsigned long zbar_fourcc_parse (const char *format) {
    unsigned long fourcc = 0;
    if(format) {
        int i;
        for(i = 0; i < 4 && format[i]; i++)
            fourcc |= ((unsigned long)format[i]) << (i * 8);
    }
    return(fourcc);
}

extern int _zbar_error_spew(const void *object, int verbosity);
extern const char *_zbar_error_string(const void *object, int verbosity);
extern zbar_error_t _zbar_get_error_code(const void *object);

struct zbar_symbol_s;
typedef struct zbar_symbol_s zbar_symbol_t;

struct zbar_symbol_set_s;
typedef struct zbar_symbol_set_s zbar_symbol_set_t;

extern void zbar_symbol_ref(const zbar_symbol_t *symbol,int refs);

extern zbar_symbol_type_t zbar_symbol_get_type(const zbar_symbol_t *symbol);

extern unsigned int zbar_symbol_get_configs(const zbar_symbol_t *symbol);

extern unsigned int zbar_symbol_get_modifiers(const zbar_symbol_t *symbol);

extern const char *zbar_symbol_get_data(const zbar_symbol_t *symbol);
extern unsigned int zbar_symbol_get_data_length(const zbar_symbol_t *symbol);
extern int zbar_symbol_get_quality(const zbar_symbol_t *symbol);

extern int zbar_symbol_get_count(const zbar_symbol_t *symbol);

extern unsigned zbar_symbol_get_loc_size(const zbar_symbol_t *symbol);

extern int zbar_symbol_get_loc_x(const zbar_symbol_t *symbol, unsigned index);

extern int zbar_symbol_get_loc_y(const zbar_symbol_t *symbol, unsigned index);

extern zbar_orientation_t zbar_symbol_get_orientation(const zbar_symbol_t *symbol);

extern const zbar_symbol_t *zbar_symbol_next(const zbar_symbol_t *symbol);

extern const zbar_symbol_set_t* zbar_symbol_get_components(const zbar_symbol_t *symbol);

extern const zbar_symbol_t* zbar_symbol_first_component(const zbar_symbol_t *symbol);

extern char *zbar_symbol_xml(const zbar_symbol_t *symbol, char **buffer, unsigned *buflen);

extern void zbar_symbol_set_ref(const zbar_symbol_set_t *symbols,int refs);
extern int zbar_symbol_set_get_size(const zbar_symbol_set_t *symbols);

extern const zbar_symbol_t*
zbar_symbol_set_first_symbol(const zbar_symbol_set_t *symbols);
extern const zbar_symbol_t*
zbar_symbol_set_first_unfiltered(const zbar_symbol_set_t *symbols);

struct zbar_image_s;
typedef struct zbar_image_s zbar_image_t;

typedef void (zbar_image_cleanup_handler_t)(zbar_image_t *image);

typedef void (zbar_image_data_handler_t)(zbar_image_t *image,const void *userdata);

extern zbar_image_t *zbar_image_create(void);

extern void zbar_image_destroy(zbar_image_t *image);

extern void zbar_image_ref(zbar_image_t *image, int refs);

extern zbar_image_t *zbar_image_convert(const zbar_image_t *image,unsigned long format);

extern zbar_image_t *zbar_image_convert_resize(const zbar_image_t *image,
                                               unsigned long format,
                                               unsigned width,
                                               unsigned height);
extern unsigned long zbar_image_get_format(const zbar_image_t *image);

extern unsigned zbar_image_get_sequence(const zbar_image_t *image);
extern unsigned zbar_image_get_width(const zbar_image_t *image);

extern unsigned zbar_image_get_height(const zbar_image_t *image);

extern void zbar_image_get_size(const zbar_image_t *image,
                                unsigned *width,
                                unsigned *height);

extern void zbar_image_get_crop(const zbar_image_t *image,
                                unsigned *x,
                                unsigned *y,
                                unsigned *width,
                                unsigned *height);

extern const void *zbar_image_get_data(const zbar_image_t *image);

extern unsigned long zbar_image_get_data_length(const zbar_image_t *img);

extern const zbar_symbol_set_t* zbar_image_get_symbols(const zbar_image_t *image);

extern void zbar_image_set_symbols(zbar_image_t *image,const zbar_symbol_set_t *symbols);

extern const zbar_symbol_t* zbar_image_first_symbol(const zbar_image_t *image);

extern void zbar_image_set_format(zbar_image_t *image,unsigned long format);

extern void zbar_image_set_sequence(zbar_image_t *image,unsigned sequence_num);

extern void zbar_image_set_size(zbar_image_t *image,
                                unsigned width,
                                unsigned height);

extern void zbar_image_set_crop(zbar_image_t *image,
                                unsigned x,
                                unsigned y,
                                unsigned width,
                                unsigned height);

extern void zbar_image_set_data(zbar_image_t *image, const void *data,unsigned long data_byte_length, zbar_image_cleanup_handler_t *cleanup_hndlr);

extern void zbar_image_free_data(zbar_image_t *image);

extern void zbar_image_set_userdata(zbar_image_t *image, void *userdata);

extern void *zbar_image_get_userdata(const zbar_image_t *image);

extern int zbar_image_write(const zbar_image_t *image, const char *filebase);

extern zbar_image_t *zbar_image_read(char *filename);

struct zbar_processor_s;
typedef struct zbar_processor_s zbar_processor_t;

extern zbar_processor_t *zbar_processor_create(int threaded);

extern void zbar_processor_destroy(zbar_processor_t *processor);

extern int zbar_processor_init(zbar_processor_t *processor, const char *video_device, int enable_display);

extern int zbar_processor_request_size(zbar_processor_t *processor,unsigned width, unsigned height);

extern int zbar_processor_request_interface(zbar_processor_t *processor,int version);

extern int zbar_processor_request_iomode(zbar_processor_t *video, int iomode);

extern int zbar_processor_force_format(zbar_processor_t *processor, unsigned long input_format, unsigned long output_format);

extern zbar_image_data_handler_t* zbar_processor_set_data_handler(zbar_processor_t *processor, zbar_image_data_handler_t *handler, const void *userdata);

extern void zbar_processor_set_userdata(zbar_processor_t *processor, void *userdata);

extern void *zbar_processor_get_userdata(const zbar_processor_t *processor);

extern int zbar_processor_set_config(zbar_processor_t *processor,zbar_symbol_type_t symbology, zbar_config_t config,int value);

static inline int zbar_processor_parse_config (zbar_processor_t *processor, const char *config_string) {
    zbar_symbol_type_t sym;
    zbar_config_t cfg;
    int val;
    return(zbar_parse_config(config_string, &sym, &cfg, &val) ||
           zbar_processor_set_config(processor, sym, cfg, val));
}

extern int zbar_processor_is_visible(zbar_processor_t *processor);

extern int zbar_processor_set_visible(zbar_processor_t *processor,int visible);

extern int zbar_processor_set_active(zbar_processor_t *processor, int active);

extern const zbar_symbol_set_t* zbar_processor_get_results(const zbar_processor_t *processor);

extern int zbar_processor_user_wait(zbar_processor_t *processor, int timeout);

extern int zbar_process_one(zbar_processor_t *processor, int timeout);

extern int zbar_process_image(zbar_processor_t *processor, zbar_image_t *image);

static inline int
zbar_processor_error_spew (const zbar_processor_t *processor, int verbosity) {
    return(_zbar_error_spew(processor, verbosity));
}

static inline const char* zbar_processor_error_string (const zbar_processor_t *processor, int verbosity) {
    return(_zbar_error_string(processor, verbosity));
}

static inline zbar_error_t zbar_processor_get_error_code (const zbar_processor_t *processor) {
    return(_zbar_get_error_code(processor));
}

struct zbar_video_s;
typedef struct zbar_video_s zbar_video_t;

extern zbar_video_t *zbar_video_create(void);

extern void zbar_video_destroy(zbar_video_t *video);

extern int zbar_video_open(zbar_video_t *video, const char *device);

extern int zbar_video_get_fd(const zbar_video_t *video);

extern int zbar_video_request_size(zbar_video_t *video, unsigned width, unsigned height);

extern int zbar_video_request_interface(zbar_video_t *video, int version);

extern int zbar_video_request_iomode(zbar_video_t *video, int iomode);

extern int zbar_video_get_width(const zbar_video_t *video);

extern int zbar_video_get_height(const zbar_video_t *video);

extern int zbar_video_init(zbar_video_t *video, unsigned long format);

extern int zbar_video_enable(zbar_video_t *video, int enable);

extern zbar_image_t *zbar_video_next_image(zbar_video_t *video);

static inline int zbar_video_error_spew (const zbar_video_t *video, int verbosity) {
    return(_zbar_error_spew(video, verbosity));
}

static inline const char *zbar_video_error_string (const zbar_video_t *video, int verbosity) {
    return(_zbar_error_string(video, verbosity));
}

static inline zbar_error_t zbar_video_get_error_code (const zbar_video_t *video) {
    return(_zbar_get_error_code(video));
}

struct zbar_window_s;
typedef struct zbar_window_s zbar_window_t;

extern zbar_window_t *zbar_window_create(void);

extern void zbar_window_destroy(zbar_window_t *window);
extern int zbar_window_attach(zbar_window_t *window, void *x11_display_w32_hwnd, unsigned long x11_drawable);

extern void zbar_window_set_overlay(zbar_window_t *window, int level);

extern int zbar_window_get_overlay(const zbar_window_t *window);

extern int zbar_window_draw(zbar_window_t *window, zbar_image_t *image);

extern int zbar_window_redraw(zbar_window_t *window);

extern int zbar_window_resize(zbar_window_t *window, unsigned width, unsigned height);

static inline int zbar_window_error_spew (const zbar_window_t *window, int verbosity) {
    return(_zbar_error_spew(window, verbosity));
}

static inline const char*
zbar_window_error_string (const zbar_window_t *window, int verbosity) {
    return(_zbar_error_string(window, verbosity));
}

static inline zbar_error_t
zbar_window_get_error_code (const zbar_window_t *window) {
    return(_zbar_get_error_code(window));
}


extern int zbar_negotiate_format(zbar_video_t *video, zbar_window_t *window);

struct zbar_image_scanner_s;
typedef struct zbar_image_scanner_s zbar_image_scanner_t;

extern zbar_image_scanner_t *zbar_image_scanner_create(void);

extern void zbar_image_scanner_destroy(zbar_image_scanner_t *scanner);
extern zbar_image_data_handler_t*
zbar_image_scanner_set_data_handler(zbar_image_scanner_t *scanner, zbar_image_data_handler_t *handler, const void *userdata);


extern int zbar_image_scanner_set_config(zbar_image_scanner_t *scanner, zbar_symbol_type_t symbology, zbar_config_t config, int value);

static inline int
zbar_image_scanner_parse_config (zbar_image_scanner_t *scanner, const char *config_string)
{
    zbar_symbol_type_t sym;
    zbar_config_t cfg;
    int val;
    return(zbar_parse_config(config_string, &sym, &cfg, &val) ||
           zbar_image_scanner_set_config(scanner, sym, cfg, val));
}

extern void zbar_image_scanner_enable_cache(zbar_image_scanner_t *scanner, int enable);

extern void zbar_image_scanner_recycle_image(zbar_image_scanner_t *scanner,
                                             zbar_image_t *image);

extern const zbar_symbol_set_t*
zbar_image_scanner_get_results(const zbar_image_scanner_t *scanner);

extern int zbar_scan_image(zbar_image_scanner_t *scanner,
                           zbar_image_t *image);

struct zbar_decoder_s;
typedef struct zbar_decoder_s zbar_decoder_t;

typedef void (zbar_decoder_handler_t)(zbar_decoder_t *decoder);

extern zbar_decoder_t *zbar_decoder_create(void);

extern void zbar_decoder_destroy(zbar_decoder_t *decoder);

extern int zbar_decoder_set_config(zbar_decoder_t *decoder, zbar_symbol_type_t symbology, zbar_config_t config, int value);

static inline int zbar_decoder_parse_config (zbar_decoder_t *decoder, const char *config_string)
{
    zbar_symbol_type_t sym;
    zbar_config_t cfg;
    int val;
    return(zbar_parse_config(config_string, &sym, &cfg, &val) ||
           zbar_decoder_set_config(decoder, sym, cfg, val));
}

extern unsigned int zbar_decoder_get_configs(const zbar_decoder_t *decoder, zbar_symbol_type_t symbology);

extern void zbar_decoder_reset(zbar_decoder_t *decoder);

extern void zbar_decoder_new_scan(zbar_decoder_t *decoder);

extern zbar_symbol_type_t zbar_decode_width(zbar_decoder_t *decoder, unsigned width);

extern zbar_color_t zbar_decoder_get_color(const zbar_decoder_t *decoder);

extern const char *zbar_decoder_get_data(const zbar_decoder_t *decoder);

extern unsigned int zbar_decoder_get_data_length(const zbar_decoder_t *decoder);

extern zbar_symbol_type_t zbar_decoder_get_type(const zbar_decoder_t *decoder);

extern unsigned int zbar_decoder_get_modifiers(const zbar_decoder_t *decoder);

extern int zbar_decoder_get_direction(const zbar_decoder_t *decoder);

extern zbar_decoder_handler_t* zbar_decoder_set_handler(zbar_decoder_t *decoder, zbar_decoder_handler_t *handler);

extern void zbar_decoder_set_userdata(zbar_decoder_t *decoder, void *userdata);

extern void *zbar_decoder_get_userdata(const zbar_decoder_t *decoder);

struct zbar_scanner_s;
typedef struct zbar_scanner_s zbar_scanner_t;
extern zbar_scanner_t *zbar_scanner_create(zbar_decoder_t *decoder);

extern void zbar_scanner_destroy(zbar_scanner_t *scanner);
extern zbar_symbol_type_t zbar_scanner_reset(zbar_scanner_t *scanner);
extern zbar_symbol_type_t zbar_scanner_new_scan(zbar_scanner_t *scanner);
extern zbar_symbol_type_t zbar_scanner_flush(zbar_scanner_t *scanner);
extern zbar_symbol_type_t zbar_scan_y(zbar_scanner_t *scanner, int y);
static inline zbar_symbol_type_t zbar_scan_rgb24 (zbar_scanner_t *scanner, unsigned char *rgb) {
    return(zbar_scan_y(scanner, rgb[0] + rgb[1] + rgb[2]));
}

extern unsigned zbar_scanner_get_width(const zbar_scanner_t *scanner);
extern unsigned zbar_scanner_get_edge(const zbar_scanner_t *scn, unsigned offset, int prec);

extern zbar_color_t zbar_scanner_get_color(const zbar_scanner_t *scanner);

#ifdef __cplusplus
    }
}

# include "zbar/Exception.h"
# include "zbar/Decoder.h"
# include "zbar/Scanner.h"
# include "zbar/Symbol.h"
# include "zbar/Image.h"
# include "zbar/ImageScanner.h"
# include "zbar/Video.h"
# include "zbar/Window.h"
# include "zbar/Processor.h"
#endif

#endif
