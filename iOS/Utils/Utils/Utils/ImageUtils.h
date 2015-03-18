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

const float matrix_lomo[] = {
    1.7f,  0.1f, 0.1f, 0, -73.1f,
    0,  1.7f, 0.1f, 0, -73.1f,
    0,  0.1f, 1.6f, 0, -73.1f,
    0,  0, 0, 1.0f, 0 };

const float matrix_blackwhite[] = {
    0.8f,  1.6f, 0.2f, 0, -163.9f,
    0.8f,  1.6f, 0.2f, 0, -163.9f,
    0.8f,  1.6f, 0.2f, 0, -163.9f,
    0,  0, 0, 1.0f, 0 };

const float matrix_old[] = {
    0.2f,0.5f, 0.1f, 0, 40.8f,
    0.2f, 0.5f, 0.1f, 0, 40.8f,
    0.2f,0.5f, 0.1f, 0, 40.8f,
    0, 0, 0, 1, 0 };

const float matrix_light[] = {
    0.6f,0.3f, 0.1f, 0,73.3f,
    0.2f,0.7f, 0.1f, 0,73.3f,
    0.2f,0.3f, 0.4f, 0,73.3f,
    0, 0, 0, 1.0f, 0 };

const float matrix_quite[] = {
    0.9f, 0, 0, 0, 0,
    0, 1.1f,0, 0, 0,
    0, 0, 0.9f, 0, 0,
    0, 0, 0, 1.0f, 0 };

const float matrix_roman[] = {
    0.9f, 0, 0, 0, 63.0f,
    0, 0.9f,0, 0, 63.0f,
    0, 0, 0.9f, 0, 63.0f,
    0, 0, 0, 1.0f, 0 };

const float matrix_halo[] = {
    0.9f, 0, 0,  0, 64.9f,
    0, 0.9f,0,  0, 64.9f,
    0, 0, 0.9f,  0, 64.9f,
    0, 0, 0, 1.0f, 0 };

const float matrix_dream[] = {
    0.8f, 0.3f, 0.1f, 0.0f, 46.5f,
    0.1f, 0.9f, 0.0f, 0.0f, 46.5f,
    0.1f, 0.3f, 0.7f, 0.0f, 46.5f,
    0.0f, 0.0f, 0.0f, 1.0f, 0.0f
};

@interface ImageUtils : NSObject

+(ImageSize) getImageSize: (UIImage *) image;
+ (RGBAColor) getPixelColor:(CGPoint)point size:(ImageSize)size imageData:(Byte *)data;
+(void) releaseData: (Byte *) imageData;
+(Byte *) getImageData: (UIImage *) image;
+(UIEdgeInsets) get9PatchEdge: (ImageSize) size imageData: (Byte *) data;
+(UIImage *) cut9Patch: (UIImage *) image;

+(UIImage *) load9PatchImage: (UIImage *)image;
+(UIImage *) scaleImage: (UIImage *)image scale: (CGFloat)scale;

+(UIImage *) loadFromFile: (NSString *)file;
+(void) saveToFile:(UIImage *)image file:(NSString *)file format:(NSString *)format;
+(UIImage *) loadFromAssets: (NSString *)name type:(NSString *)type;

+(UIImage *)roundedCornerImage:(UIImage*)image radius:(NSInteger)radius;
+(UIImage *)blackWhiteImage: (UIImage *)image;
+(UIImage *)colorMatrix:(UIImage*)image colorMatrix:(const float*) f;
+(UIImage *)blurImage:(UIImage *)image blur:(CGFloat)blur;

+(UIImage*)rotate90Clockwise:(UIImage *)image;
+(UIImage*)rotate90CounterClockwise:(UIImage *)image;
+(UIImage*)rotate180:(UIImage *)image;
+(UIImage*)rotateImageToOrientationUp:(UIImage *)image;
+(UIImage*)flipHorizontal:(UIImage *)image;
+(UIImage*)flipVertical:(UIImage *)image;
+(UIImage*)flipAll:(UIImage *)image;

@end
