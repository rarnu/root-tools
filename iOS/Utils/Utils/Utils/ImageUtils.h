#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

typedef struct {
    size_t width;
    size_t height;
} ImageSize;

typedef struct {
    int red;
    int green;
    int blue;
    int alpha;
} RGBAColor;


@interface ImageUtils : NSObject

+(ImageSize) getImageSize: (UIImage *) image;
+ (RGBAColor) getPixelColor:(CGPoint)point size:(ImageSize)size imageData:(Byte *)data;
+(void) releaseData: (Byte *) imageData;
+(Byte *) getImageData: (UIImage *) image;
+(UIEdgeInsets) get9PatchEdge: (ImageSize) size imageData: (Byte *) data;
+(UIImage *) cut9Patch: (UIImage *) image;

+(UIImage *) load9PatchImage: (UIImage *)image;
+(UIImage *) scaleImage: (UIImage *)image scale: (CGFloat)scale;

@end
