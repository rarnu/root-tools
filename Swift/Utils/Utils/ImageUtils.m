#import "ImageUtils.h"

@implementation ImageUtils

+(ImageSize) getImageSize: (UIImage *) image {
    CGImageRef imgRef = image.CGImage;
    ImageSize size;
    size.width = CGImageGetWidth(imgRef);
    size.height = CGImageGetHeight(imgRef);
    return size;
}

+ (RGBAColor) getPixelColor:(CGPoint)point size:(ImageSize)size imageData:(Byte *)data {
    RGBAColor color;
    if (data != NULL) {
        int offset = 4 * ((size.width * round(point.y))+round(point.x));
        color.red = data[offset];
        color.green = data[offset+1];
        color.blue = data[offset+2];
        color.alpha =  data[offset+3];
    }
    return color;
}

+(void) releaseData: (Byte *) imageData {
    if (imageData) {
        free(imageData);
    }
}

+(Byte *) getImageData: (UIImage *) image {
    CFDataRef bitmapData = CGDataProviderCopyData(CGImageGetDataProvider(image.CGImage));
    Byte * bytes = (Byte *)CFDataGetBytePtr(bitmapData);
    return bytes;
}

+(UIEdgeInsets) get9PatchEdge: (ImageSize) size imageData: (Byte *) data {
    RGBAColor color;
    int left = 0;
    int right = 0;
    int top = 0;
    int bottom = 0;
    BOOL pre = NO;
    for (int i = 0; i < size.width; i++) {
        color = [self getPixelColor:CGPointMake(i, 0) size:size imageData:data];
        if (color.alpha == 0) {
            if (pre) {
                right = i - 1;
            }
            pre = NO;
        } else {
            if (!pre) {
                left = i;
            }
            pre = YES;
        }
    }
    
    pre = NO;
    
    for (int i = 0; i < size.height; i++) {
        color = [self getPixelColor:CGPointMake(0, i) size:size imageData:data];
        if (color.alpha == 0) {
            if (pre) {
                bottom = i - 1;
            }
            pre = NO;
        } else {
            if (!pre) {
                top = i;
            }
            pre = YES;
        }
    }
    return UIEdgeInsetsMake(top, left, bottom, right);
}

+(UIImage *) cut9Patch: (UIImage *) image {
    CGSize size = image.size;
    CGRect rect = CGRectMake(1, 1, size.width - 2, size.height - 2);
    CGImageRef newImgRef =  CGImageCreateWithImageInRect([image CGImage], rect);
    UIImage * cropped = [UIImage imageWithCGImage:newImgRef];
    CGImageRelease(newImgRef);
    return cropped;
}

+(UIImage *) load9PatchImage: (UIImage *) image {
    ImageSize size = [self getImageSize: image];
    Byte * data = [self getImageData:image];
    UIEdgeInsets edge = [self get9PatchEdge:size imageData:data];
    [self releaseData:data];
    UIImage * cropped = [self cut9Patch:image];
    cropped = [cropped resizableImageWithCapInsets:edge resizingMode:UIImageResizingModeStretch];
    return cropped;
}

+(UIImage *) scaleImage: (UIImage *)image scale: (CGFloat)scale {
    CGFloat w = image.size.width;
    CGFloat h = image.size.height;
    CGFloat newW = w * scale;
    CGFloat newH = h * scale;
    CGSize newSize = CGSizeMake(newW, newH);
    UIGraphicsBeginImageContext(newSize);
    [image drawInRect: CGRectMake(0, 0, newSize.width, newSize.height)];
    image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return image;

}

@end
