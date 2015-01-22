#import <UIKit/UIKit.h>
#import "SideMenu.h"

@class SideMenu;

@interface UIViewController (SideMenu)

@property (strong, readonly, nonatomic) SideMenu *sideMenuViewController;

- (IBAction)presentLeftMenuViewController:(id)sender;
- (IBAction)presentRightMenuViewController:(id)sender;

@end
