#import <CoreGraphics/CoreGraphics.h>
#import "ZBarImageScanner.h"

@class AVCaptureVideoDataOutput, AVCaptureOutput;
@class ZBarCaptureReader, ZBarCVImage;

@protocol ZBarCaptureDelegate <NSObject>

- (void)       captureReader: (ZBarCaptureReader*) captureReader
  didReadNewSymbolsFromImage: (ZBarImage*) image;

@optional

- (void) captureReader: (ZBarCaptureReader*) captureReader
       didTrackSymbols: (ZBarSymbolSet*) symbols;

@end

@interface ZBarCaptureReader : NSObject {
#if !TARGET_IPHONE_SIMULATOR
    AVCaptureVideoDataOutput *captureOutput;
    id<ZBarCaptureDelegate> captureDelegate;
    ZBarImageScanner *scanner;
    CGRect scanCrop;
    CGSize size;
    CGFloat framesPerSecond;
    BOOL enableCache;

    dispatch_queue_t queue;
    ZBarImage *image;
    ZBarCVImage *result;
    volatile uint32_t state;
    int framecnt;
    unsigned width, height;
    uint64_t t_frame, t_fps, t_scan;
    CGFloat dt_frame;
#endif
}


- (id) initWithImageScanner: (ZBarImageScanner*) imageScanner;
- (void) willStartRunning;
- (void) willStopRunning;
- (void) flushCache;
- (void) captureFrame;
@property (nonatomic, readonly) AVCaptureOutput *captureOutput;
@property (nonatomic, assign) id<ZBarCaptureDelegate> captureDelegate;
@property (nonatomic, readonly) ZBarImageScanner *scanner;
@property (nonatomic, assign) CGRect scanCrop;
@property (nonatomic, readonly) CGSize size;
@property (nonatomic) BOOL enableReader;
@property (nonatomic, readonly) CGFloat framesPerSecond;

@property (nonatomic) BOOL enableCache;

@end
