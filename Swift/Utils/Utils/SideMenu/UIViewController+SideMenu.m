#import "UIViewController+SideMenu.h"

@implementation UIViewController (SideMenu)

- (SideMenu *)sideMenuViewController {
    UIViewController *iter = self.parentViewController;
    while (iter) {
        if ([iter isKindOfClass:[SideMenu class]]) {
            return (SideMenu *)iter;
        } else if (iter.parentViewController && iter.parentViewController != iter) {
            iter = iter.parentViewController;
        } else {
            iter = nil;
        }
    }
    return nil;
}

- (IBAction)presentLeftMenuViewController:(id)sender {
    [self.sideMenuViewController presentLeftMenuViewController];
}

- (IBAction)presentRightMenuViewController:(id)sender {
    [self.sideMenuViewController presentRightMenuViewController];
}

@end
