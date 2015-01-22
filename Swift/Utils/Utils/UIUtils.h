#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

typedef struct {
    CGFloat top;
    CGFloat remainHeight;
} TitleInfo;

@interface UIUtils : NSObject

+(UIColor *) getDefaultColor;
+(CGSize) getStatusBarSize;
+(CGSize) getScreenSize;
+(void) setNavBar: (UINavigationBar *)nav bgColor: (UIColor *)bgColor textColor: (UIColor *)textColor;
+(void) setTabBar: (UITabBar *)tab bgColor: (UIColor *)bgColor textColor: (UIColor *)textColor;
+(void) setStatusBarStyle: (BOOL)light;
+(CGFloat) getStatusBarHeight;
+(CGSize) getAppFrameSize;
+(CGFloat) getNavigationBarHeight: (UINavigationBar *)nav;
+(TitleInfo) setTextTitleForSideMenu: (UIViewController *)controller nav: (UINavigationBar *)nav txt: (UILabel *)txt;

@end
