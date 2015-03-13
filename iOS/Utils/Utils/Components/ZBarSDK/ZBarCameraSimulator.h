@class ZBarReaderView;

@interface ZBarCameraSimulator : NSObject < UINavigationControllerDelegate, UIImagePickerControllerDelegate, UIPopoverControllerDelegate > {
    UIViewController *viewController;
    ZBarReaderView *readerView;
    UIImagePickerController *picker;
    UIPopoverController *pickerPopover;
}

- (id) initWithViewController: (UIViewController*) viewController;
- (void) takePicture;

@property (nonatomic, assign) ZBarReaderView *readerView;

@end
