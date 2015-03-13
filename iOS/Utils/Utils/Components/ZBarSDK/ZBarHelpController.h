#import <UIKit/UIKit.h>

@class ZBarHelpController;

@protocol ZBarHelpDelegate
@optional

- (void) helpControllerDidFinish: (ZBarHelpController*) help;

@end

@interface ZBarHelpController : UIViewController< UIWebViewDelegate, UIAlertViewDelegate> {
    NSString *reason;
    id delegate;
    UIWebView *webView;
    UIToolbar *toolbar;
    UIBarButtonItem *doneBtn, *backBtn, *space;
    NSURL *linkURL;
    NSUInteger orientations;
}

@property (nonatomic, assign) id<ZBarHelpDelegate> delegate;

- (id) initWithReason: (NSString*) reason;

- (BOOL) isInterfaceOrientationSupported: (UIInterfaceOrientation) orientation;
- (void) setInterfaceOrientation: (UIInterfaceOrientation) orientation supported: (BOOL) supported;

@end
