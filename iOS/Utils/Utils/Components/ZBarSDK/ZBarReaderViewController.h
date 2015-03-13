#import <UIKit/UIKit.h>
#import "ZBarReaderController.h"

#define ZBarOrientationMask(orient) (1 << orient)
#define ZBarOrientationMaskAll \
    (ZBarOrientationMask(UIInterfaceOrientationPortrait) | \
     ZBarOrientationMask(UIInterfaceOrientationPortraitUpsideDown) | \
     ZBarOrientationMask(UIInterfaceOrientationLandscapeLeft) | \
     ZBarOrientationMask(UIInterfaceOrientationLandscapeRight))

@class ZBarReaderView, ZBarCameraSimulator;

@interface ZBarReaderViewController : UIViewController {
    ZBarImageScanner *scanner;
    id <ZBarReaderDelegate> readerDelegate;
    ZBarReaderView *readerView;
    UIView *cameraOverlayView;
    CGAffineTransform cameraViewTransform;
    CGRect scanCrop;
    NSUInteger supportedOrientationsMask;
    UIImagePickerControllerCameraDevice cameraDevice;
    UIImagePickerControllerCameraFlashMode cameraFlashMode;
    UIImagePickerControllerQualityType videoQuality;
    BOOL showsZBarControls, tracksSymbols, enableCache;

    ZBarHelpController *helpController;
    UIView *controls, *shutter;
    BOOL didHideStatusBar, rotating;
    ZBarCameraSimulator *cameraSim;
}

@property (nonatomic, readonly) ZBarImageScanner *scanner;
@property (nonatomic, assign) id <ZBarReaderDelegate> readerDelegate;
@property (nonatomic) BOOL showsZBarControls;
@property (nonatomic) BOOL tracksSymbols;
@property (nonatomic) NSUInteger supportedOrientationsMask;
@property (nonatomic) CGRect scanCrop;
@property (nonatomic, retain) UIView *cameraOverlayView;
@property (nonatomic) CGAffineTransform cameraViewTransform;
- (void) showHelpWithReason: (NSString*) reason;
- (void) takePicture;
+ (BOOL) isCameraDeviceAvailable: (UIImagePickerControllerCameraDevice) cameraDevice;
+ (BOOL) isFlashAvailableForCameraDevice: (UIImagePickerControllerCameraDevice) cameraDevice;
+ (NSArray*) availableCaptureModesForCameraDevice: (UIImagePickerControllerCameraDevice) cameraDevice;
@property(nonatomic) UIImagePickerControllerCameraDevice cameraDevice;
@property(nonatomic) UIImagePickerControllerCameraFlashMode cameraFlashMode;
@property(nonatomic) UIImagePickerControllerCameraCaptureMode cameraCaptureMode;
@property(nonatomic) UIImagePickerControllerQualityType videoQuality;
@property (nonatomic, readonly) ZBarReaderView *readerView;
@property (nonatomic) BOOL enableCache;
@property (nonatomic) UIImagePickerControllerSourceType sourceType; // Camera
@property (nonatomic) BOOL allowsEditing; // NO
@property (nonatomic) BOOL allowsImageEditing; // NO
@property (nonatomic) BOOL showsCameraControls; // NO
@property (nonatomic) BOOL showsHelpOnFail; // ignored
@property (nonatomic) ZBarReaderControllerCameraMode cameraMode; // Sampling
@property (nonatomic) BOOL takesPicture; // NO
@property (nonatomic) NSInteger maxScanDimension; // ignored

+ (BOOL) isSourceTypeAvailable: (UIImagePickerControllerSourceType) sourceType;

@end
