#import <UIKit/UIKit.h>
#import "ZBarImageScanner.h"

@class AVCaptureSession, AVCaptureDevice;
@class CALayer;
@class ZBarImageScanner, ZBarCaptureReader, ZBarReaderView;

@protocol ZBarReaderViewDelegate < NSObject >

- (void) readerView: (ZBarReaderView*) readerView
     didReadSymbols: (ZBarSymbolSet*) symbols
          fromImage: (UIImage*) image;

@optional
- (void) readerViewDidStart: (ZBarReaderView*) readerView;
- (void) readerView: (ZBarReaderView*) readerView
   didStopWithError: (NSError*) error;

@end

@interface ZBarReaderView : UIView {
    id<ZBarReaderViewDelegate> readerDelegate;
    ZBarCaptureReader *captureReader;
    CGRect scanCrop, effectiveCrop;
    CGAffineTransform previewTransform;
    CGFloat zoom, zoom0, maxZoom;
    UIColor *trackingColor;
    BOOL tracksSymbols, showsFPS;
    NSInteger torchMode;
    UIInterfaceOrientation interfaceOrientation;
    NSTimeInterval animationDuration;

    CALayer *preview, *overlay, *tracking, *cropLayer;
    UIView *fpsView;
    UILabel *fpsLabel;
    UIPinchGestureRecognizer *pinch;
    CGFloat imageScale;
    CGSize imageSize;
    BOOL started, running, locked;
}

- (id) initWithImageScanner: (ZBarImageScanner*) imageScanner;
- (void) start;
- (void) stop;
- (void) flushCache;
- (void) willRotateToInterfaceOrientation: (UIInterfaceOrientation) orient duration: (NSTimeInterval) duration;

@property (nonatomic, assign) id<ZBarReaderViewDelegate> readerDelegate;
@property (nonatomic, readonly) ZBarImageScanner *scanner;
@property (nonatomic) BOOL tracksSymbols;
@property (nonatomic, retain) UIColor *trackingColor;
@property (nonatomic) BOOL allowsPinchZoom;
@property (nonatomic) NSInteger torchMode;
@property (nonatomic) BOOL showsFPS;
@property (nonatomic) CGFloat zoom;
- (void) setZoom: (CGFloat) zoom animated: (BOOL) animated;

@property (nonatomic) CGFloat maxZoom;
@property (nonatomic) CGRect scanCrop;
@property (nonatomic) CGAffineTransform previewTransform;
@property (nonatomic, retain) AVCaptureDevice *device;
@property (nonatomic, readonly) AVCaptureSession *session;
@property (nonatomic, readonly) ZBarCaptureReader *captureReader;
@property (nonatomic) BOOL enableCache;

@end
