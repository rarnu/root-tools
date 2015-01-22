#import "UIUtils.h"

@implementation UIUtils

+(UIColor *) getDefaultColor {
    return [UIColor colorWithRed:0x00 green:0xA5 / 0xFF blue:0xED / 0xFF alpha:1];
}

+(CGSize) getStatusBarSize {
    return [UIApplication sharedApplication].statusBarFrame.size;
}

+(CGSize) getScreenSize {
    return [UIScreen mainScreen].bounds.size;
}

+(void) setNavBar: (UINavigationBar *)nav bgColor: (UIColor *)bgColor textColor: (UIColor *)textColor {
    nav.barTintColor = bgColor;
    nav.tintColor = textColor;
    nav.titleTextAttributes = [NSDictionary dictionaryWithObjectsAndKeys:textColor, NSForegroundColorAttributeName, nil];
}

+(void) setTabBar: (UITabBar *)tab bgColor: (UIColor *)bgColor textColor: (UIColor *)textColor {
    tab.tintColor = textColor;
    tab.barTintColor = bgColor;
}

+(void) setStatusBarStyle: (BOOL)light {
    [[UIApplication sharedApplication] setStatusBarStyle:(light ? UIStatusBarStyleLightContent : UIStatusBarStyleDefault) animated:NO];
}

+(CGFloat) getStatusBarHeight {
    return [UIApplication sharedApplication].statusBarFrame.size.height;
}

+(CGSize) getAppFrameSize {
    return [UIScreen mainScreen].applicationFrame.size;
}

+(CGFloat) getNavigationBarHeight: (UINavigationBar *)nav {
    return nav.frame.size.height;
}

+(TitleInfo) setTextTitleForSideMenu: (UIViewController *)controller nav: (UINavigationBar *)nav txt: (UILabel *)txt {
    CGFloat y = [self getStatusBarSize].height;
    CGFloat w = [self getScreenSize].width;
    CGFloat h = [self getNavigationBarHeight:nav];
    CGFloat t = y + h;
    CGFloat rh = [self getScreenSize].height - t;
    UIColor * backColor = [self getDefaultColor];
    txt.frame = CGRectMake(0, y, w, h);
    txt.backgroundColor = backColor;
    txt.textColor = [UIColor whiteColor];
    h = [self getStatusBarSize].height;
    UILabel * txtImmersion = [[UILabel alloc] initWithFrame: CGRectMake(0, 0, w, h)];
    txtImmersion.backgroundColor = backColor;
    [controller.view addSubview:txtImmersion];
    TitleInfo ret;
    ret.top = t;
    ret.remainHeight = rh;
    return ret;
}


@end
