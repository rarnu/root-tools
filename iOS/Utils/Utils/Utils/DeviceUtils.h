#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

#define BUNDLE_DISPLAY_NAME @"CFBundleDisplayName"
#define BUNDLE_SHORT_VERSION @"CFBundleShortVersionString"
#define BUNDLE_VERSION @"CFBundleVersion"

@interface DeviceInfo : NSObject

@property NSString * deviceName;
@property NSString * uuid;
@property NSString * system;
@property NSString * systemVersion;
@property NSString * model;
@property NSString * appName;
@property NSString * appVersion;
@property NSString * appBuild;

@end

@interface DeviceUtils : NSObject

+(DeviceInfo *) getDevieInfo;

@end
