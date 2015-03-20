#import "PermissionUtils.h"
#import <AVFoundation/AVFoundation.h>
#import <CoreLocation/CoreLocation.h>
#import <UIKit/UIKit.h>
#import "FileUtils.h"

@implementation PermissionUtils

+(BOOL)canVisitCamera {
    BOOL ret = YES;
    NSString * mediaType = AVMediaTypeVideo;
    AVAuthorizationStatus authStatus = [AVCaptureDevice authorizationStatusForMediaType:mediaType];
    if(authStatus == AVAuthorizationStatusRestricted || authStatus == AVAuthorizationStatusDenied){
        ret = NO;
    }
    return ret;
}

+(BOOL)canVisitGPS {
    return [CLLocationManager locationServicesEnabled];
}

+(BOOL)registerNotificationPermission {
    BOOL ret = NO;
    @try {
        if ([UIApplication instancesRespondToSelector:@selector(registerUserNotificationSettings:)]){
            [[UIApplication sharedApplication] registerUserNotificationSettings:[UIUserNotificationSettings settingsForTypes:UIUserNotificationTypeAlert|UIUserNotificationTypeBadge|UIUserNotificationTypeSound categories:nil]];
            ret = YES;
        }
    }
    @catch (NSException *exception) {
    }
    return ret;
}

@end
