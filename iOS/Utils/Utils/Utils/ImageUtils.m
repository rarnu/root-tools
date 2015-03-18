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

+(UIImage *) loadFromFile: (NSString *)file {
    return [UIImage imageWithContentsOfFile:file];
}

+(void) saveToFile:(UIImage *)image file:(NSString *)file format:(NSString *)format {
    if ([format isEqualToString:@"jpg"]) {
        [UIImageJPEGRepresentation(image, 1.0) writeToFile:file atomically:YES];
    } else if ([format isEqualToString:@"png"]) {
        [UIImagePNGRepresentation(image) writeToFile:file atomically:YES];
    }
}

+(UIImage *) loadFromAssets: (NSString *)name type:(NSString *)type {
    NSString * filePath = [[NSBundle mainBundle] pathForResource:name ofType:type];
    return [UIImage imageWithContentsOfFile:filePath];
}

+(CGContextRef)CreateRGBABitmapContext:(CGImageRef)inImage {
    CGContextRef context = NULL;
    CGColorSpaceRef colorSpace;
    void *bitmapData;
    int bitmapByteCount;
    int bitmapBytesPerRow;
    size_t pixelsWide = CGImageGetWidth(inImage);
    size_t pixelsHigh = CGImageGetHeight(inImage);
    bitmapBytesPerRow = (int)(pixelsWide * 4);
    bitmapByteCount = (int)(bitmapBytesPerRow * pixelsHigh);
    colorSpace = CGColorSpaceCreateDeviceRGB();
    bitmapData = malloc( bitmapByteCount );
    context = CGBitmapContextCreate (bitmapData, pixelsWide, pixelsHigh, 8, bitmapBytesPerRow, colorSpace, kCGBitmapAlphaInfoMask);
    CGColorSpaceRelease( colorSpace ); 
    return context;
}

+(char *)RequestImagePixelData:(UIImage *)inImage {
    CGImageRef img = [inImage CGImage];
    CGSize size = [inImage size];
    CGContextRef cgctx = [self CreateRGBABitmapContext:img];
    CGRect rect = {{0,0},{size.width, size.height}};
    CGContextDrawImage(cgctx, rect, img);
    char * data = CGBitmapContextGetData (cgctx);
    CGContextRelease(cgctx);
    return data;
}


+(void) addRoundedRectToPath:(CGContextRef)context rect:(CGRect)rect ovalWidth:(float)ovalWidth ovalHeight:(float)ovalHeight {
    float fw, fh;
    if (ovalWidth == 0 || ovalHeight == 0) {
        CGContextAddRect(context, rect);
        return;
    }
    CGContextSaveGState(context);
    CGContextTranslateCTM(context, CGRectGetMinX(rect), CGRectGetMinY(rect));
    CGContextScaleCTM(context, ovalWidth, ovalHeight);
    fw = CGRectGetWidth(rect) / ovalWidth;
    fh = CGRectGetHeight(rect) / ovalHeight;
    
    CGContextMoveToPoint(context, fw, fh/2);  // Start at lower right corner
    CGContextAddArcToPoint(context, fw, fh, fw/2, fh, 1);  // Top right corner
    CGContextAddArcToPoint(context, 0, fh, 0, fh/2, 1); // Top left corner
    CGContextAddArcToPoint(context, 0, 0, fw/2, 0, 1); // Lower left corner
    CGContextAddArcToPoint(context, fw, 0, fw, fh/2, 1); // Back to lower right
    
    CGContextClosePath(context);
    CGContextRestoreGState(context);
}

+(void)changeRGBA:(int *)red green:(int *)green blue:(int *)blue alpha:(int *)alpha f:(const float *)f {
    int redV=*red;
    int greenV=*green;
    int blueV=*blue;
    int alphaV=*alpha;
    *red=f[0]*redV+f[1]*greenV+f[2]*blueV+f[3]*alphaV+f[4];
    *green=f[0+5]*redV+f[1+5]*greenV+f[2+5]*blueV+f[3+5]*alphaV+f[4+5];
    *blue=f[0+5*2]*redV+f[1+5*2]*greenV+f[2+5*2]*blueV+f[3+5*2]*alphaV+f[4+5*2];
    *alpha=f[0+5*3]*redV+f[1+5*3]*greenV+f[2+5*3]*blueV+f[3+5*3]*alphaV+f[4+5*3];
    if (*red>255) {
        *red=255;
    }
    if(*red<0){
        *red=0;
    }
    if (*green>255) {
        *green=255;
    }
    if (*green<0) {
        *green=0;
    }
    if (*blue>255) {
        *blue=255;
    }
    if (*blue<0) {
        *blue=0;
    }
    if (*alpha>255) {
        *alpha=255;
    }
    if (*alpha<0) {
        *alpha=0;
    }
}

+(UIImage *)roundedCornerImage:(UIImage*)image radius:(NSInteger)radius {
    int w = image.size.width;
    int h = image.size.height;
    UIImage *img = image;
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef context = CGBitmapContextCreate(NULL, w, h, 8, 4 * w, colorSpace, kCGBitmapAlphaInfoMask);
    CGRect rect = CGRectMake(0, 0, w, h);
    CGContextBeginPath(context);
    [self addRoundedRectToPath:context rect:rect ovalWidth:radius ovalHeight:radius];
    CGContextClosePath(context);
    CGContextClip(context);
    CGContextDrawImage(context, CGRectMake(0, 0, w, h), img.CGImage);
    CGImageRef imageMasked = CGBitmapContextCreateImage(context);
    img = [UIImage imageWithCGImage:imageMasked];
    CGContextRelease(context);
    CGColorSpaceRelease(colorSpace);
    CGImageRelease(imageMasked);
    return img;
}

+ (UIImage*)colorMatrix:(UIImage*)image colorMatrix:(const float*) f {
    char * imgPixel = [self RequestImagePixelData:image];
    CGImageRef inImageRef = [image CGImage];
    GLuint w = (int)CGImageGetWidth(inImageRef);
    GLuint h = (int)CGImageGetHeight(inImageRef);
    int wOff = 0;
    int pixOff = 0;
    for(GLuint y = 0;y< h;y++) {
        pixOff = wOff;
        for (GLuint x = 0; x<w; x++) {
            int red = (unsigned char)imgPixel[pixOff];
            int green = (unsigned char)imgPixel[pixOff+1];
            int blue = (unsigned char)imgPixel[pixOff+2];
            int alpha=(unsigned char)imgPixel[pixOff+3];
            [self changeRGBA:&red green:&green blue:&blue alpha:&alpha f:f];
            imgPixel[pixOff] = red;
            imgPixel[pixOff+1] = green;
            imgPixel[pixOff+2] = blue;
            imgPixel[pixOff+3] = alpha;
            pixOff += 4;
        }
        wOff += w * 4;
    }
    
    NSInteger dataLength = w*h* 4;
    CGDataProviderRef provider = CGDataProviderCreateWithData(NULL, (char *)imgPixel, dataLength, NULL);
    int bitsPerComponent = 8;
    int bitsPerPixel = 32;
    int bytesPerRow = 4 * w;
    CGColorSpaceRef colorSpaceRef = CGColorSpaceCreateDeviceRGB();
    CGBitmapInfo bitmapInfo = kCGBitmapByteOrderDefault;
    CGColorRenderingIntent renderingIntent = kCGRenderingIntentDefault;
    CGImageRef imageRef = CGImageCreate(w, h,bitsPerComponent,bitsPerPixel,bytesPerRow,colorSpaceRef,bitmapInfo,provider,NULL, NO, renderingIntent);
    UIImage *my_Image = [UIImage imageWithCGImage:imageRef];
    CFRelease(imageRef);
    CGColorSpaceRelease(colorSpaceRef);
    CGDataProviderRelease(provider);
    return my_Image;
}

+(UIImage *)blackWhiteImage: (UIImage *)image {
    return [self colorMatrix:image colorMatrix:matrix_blackwhite];
}

+(UIImage *)blurImage:(UIImage *)image blur:(CGFloat)blur {
    CIContext *context = [CIContext contextWithOptions:nil];
    CIImage *inputImage = [[CIImage alloc] initWithImage:image];
    CIFilter *filter = [CIFilter filterWithName:@"CIGaussianBlur"];
    [filter setValue:inputImage forKey:kCIInputImageKey];
    [filter setValue:[NSNumber numberWithFloat:blur] forKey:@"inputRadius"];
    CIImage * result = [filter valueForKey:kCIOutputImageKey];
    CGImageRef cgImage = [context createCGImage:result fromRect:[result extent]];
    UIImage * ret = [UIImage imageWithCGImage:cgImage];
    CGImageRelease(cgImage);
    return ret;
}

+(UIImage*)rotate90Clockwise:(UIImage *)image {
    UIImage * ret = nil;
    switch (image.imageOrientation) {
        case UIImageOrientationUp:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRight];
            break;
        case UIImageOrientationDown:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeft];
            break;
        case UIImageOrientationLeft:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUp];
            break;
        case UIImageOrientationRight:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDown];
            break;
        case UIImageOrientationUpMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeftMirrored];
            break;
        case UIImageOrientationDownMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRightMirrored];
            break;
        case UIImageOrientationLeftMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDownMirrored];
            break;
        case UIImageOrientationRightMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUpMirrored];
            break;
        default:
            break;
    }
    
    return ret;
}

+(UIImage*)rotate90CounterClockwise:(UIImage *)image {
    UIImage * ret = nil;
    switch (image.imageOrientation) {
        case UIImageOrientationUp:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeft];
            break;
        case UIImageOrientationDown:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRight];
            break;
        case UIImageOrientationLeft:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDown];
            break;
        case UIImageOrientationRight:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUp];
            break;
        case UIImageOrientationUpMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRightMirrored];
            break;
        case UIImageOrientationDownMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeftMirrored];
            break;
        case UIImageOrientationLeftMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUpMirrored];
            break;
        case UIImageOrientationRightMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDownMirrored];
            break;
        default:
            break;
    }
    return ret;
}
+(UIImage*)rotate180:(UIImage *)image {
    UIImage * ret = nil;
    switch (image.imageOrientation) {
        case UIImageOrientationUp:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDown];
            break;
        case UIImageOrientationDown:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUp];
            break;
        case UIImageOrientationLeft:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRight];
            break;
        case UIImageOrientationRight:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeft];
            break;
        case UIImageOrientationUpMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDownMirrored];
            break;
        case UIImageOrientationDownMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUpMirrored];
            break;
        case UIImageOrientationLeftMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRightMirrored];
            break;
        case UIImageOrientationRightMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeftMirrored];
            break;
        default:
            break;
    }
    
    return ret;
}

+(UIImage*)rotateImageToOrientationUp:(UIImage *)image {
    CGSize size = CGSizeMake(image.size.width * image.scale, image.size.height * image.scale);
    UIGraphicsBeginImageContext(size);
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextClearRect(context, CGRectMake(0, 0, size.width, size.height));
    [image drawInRect:CGRectMake(0, 0, size.width, size.height)];
    UIImage *ret = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return ret;
}

+(UIImage*)flipHorizontal:(UIImage *)image {
    UIImage * ret = nil;
    switch (image.imageOrientation) {
        case UIImageOrientationUp:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUpMirrored];
            break;
        case UIImageOrientationDown:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDownMirrored];
            break;
        case UIImageOrientationLeft:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRightMirrored];
            break;
        case UIImageOrientationRight:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeftMirrored];
            break;
        case UIImageOrientationUpMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUp];
            break;
        case UIImageOrientationDownMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDown];
            break;
        case UIImageOrientationLeftMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRight];
            break;
        case UIImageOrientationRightMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeft];
            break;
        default:
            break;
    }
    
    return ret;
}

+(UIImage*)flipVertical:(UIImage *)image {
    UIImage * ret = nil;
    switch (image.imageOrientation) {
        case UIImageOrientationUp:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDownMirrored];
            break;
        case UIImageOrientationDown:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUpMirrored];
            break;
        case UIImageOrientationLeft:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeftMirrored];
            break;
        case UIImageOrientationRight:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRightMirrored];
            break;
        case UIImageOrientationUpMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationDown];
            break;
        case UIImageOrientationDownMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationUp];
            break;
        case UIImageOrientationLeftMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationLeft];
            break;
        case UIImageOrientationRightMirrored:
            ret = [UIImage imageWithCGImage:image.CGImage scale:1 orientation:UIImageOrientationRight];
            break;
        default:
            break;
    }
    return ret;
}

+(UIImage*)flipAll:(UIImage *)image {
    return [self rotate180:image];
}

@end
