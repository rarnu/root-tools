#import "DeviceUtils.h"

@implementation DeviceInfo

@end

@implementation DeviceUtils

+(DeviceInfo *) getDevieInfo {
    UIDevice * device = [UIDevice currentDevice];
    NSDictionary * dicApp = [NSBundle mainBundle].infoDictionary;
    DeviceInfo * info = [[DeviceInfo alloc] init];
    info.deviceName = device.name;
    info.uuid = device.identifierForVendor.UUIDString;
    info.system = device.systemName;
    info.systemVersion = device.systemVersion;
    info.model = device.model;
    info.appName = (NSString *)[dicApp objectForKey:BUNDLE_DISPLAY_NAME];
    info.appVersion = (NSString *)[dicApp objectForKey:BUNDLE_SHORT_VERSION];
    info.appBuild = (NSString *)[dicApp objectForKey:BUNDLE_VERSION];
    return info;
}


@end
