#import <UIKit/UIKit.h>
#import "ZBarImageScanner.h"

#ifdef __cplusplus
using namespace zbar;
#endif

typedef enum {
    ZBarReaderControllerCameraModeDefault = 0,
    ZBarReaderControllerCameraModeSampling,
    ZBarReaderControllerCameraModeSequence,

} ZBarReaderControllerCameraMode;


@class ZBarReaderController, ZBarHelpController;

@protocol ZBarReaderDelegate <UIImagePickerControllerDelegate>
@optional

- (void) readerControllerDidFailToRead: (ZBarReaderController*) reader withRetry: (BOOL) retry;

@end


@interface ZBarReaderController : UIImagePickerController <UINavigationControllerDelegate, UIImagePickerControllerDelegate> {
    ZBarImageScanner *scanner;
    ZBarHelpController *help;
    UIView *overlay, *boxView;
    CALayer *boxLayer;

    UIToolbar *toolbar;
    UIBarButtonItem *cancelBtn, *scanBtn, *space[3];
    UIButton *infoBtn;

    id <ZBarReaderDelegate> readerDelegate;
    BOOL showsZBarControls, showsHelpOnFail, takesPicture, enableCache;
    ZBarReaderControllerCameraMode cameraMode;
    CGRect scanCrop;
    NSInteger maxScanDimension;

    BOOL hasOverlay, sampling;
    uint64_t t_frame;
    double dt_frame;

    ZBarSymbol *symbol;
}

@property (readonly, nonatomic) ZBarImageScanner *scanner;
@property (nonatomic, assign) id <ZBarReaderDelegate> readerDelegate;
@property (nonatomic) BOOL showsZBarControls;
@property (nonatomic) BOOL showsHelpOnFail;
@property (nonatomic) ZBarReaderControllerCameraMode cameraMode;
@property (nonatomic) BOOL tracksSymbols;
@property (nonatomic) BOOL takesPicture;
@property (nonatomic) BOOL enableCache;
@property (nonatomic) CGRect scanCrop;
@property (nonatomic) NSInteger maxScanDimension;
- (void) showHelpWithReason: (NSString*) reason;
- (id <NSFastEnumeration>) scanImage: (CGImageRef) image;

@end

extern NSString* const ZBarReaderControllerResults;
