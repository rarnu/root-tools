#import <Foundation/Foundation.h>
#import "zbar.h"
#import "ZBarImage.h"

#ifdef __cplusplus
using namespace zbar;
#endif


@interface ZBarImageScanner : NSObject {
    zbar_image_scanner_t *scanner;
}

@property (nonatomic) BOOL enableCache;
@property (readonly, nonatomic) ZBarSymbolSet *results;

- (void) parseConfig: (NSString*) configStr;
- (void) setSymbology: (zbar_symbol_type_t) symbology config: (zbar_config_t) config to: (int) value;

- (NSInteger) scanImage: (ZBarImage*) image;

@end
