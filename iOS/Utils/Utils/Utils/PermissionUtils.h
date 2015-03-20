#import <Foundation/Foundation.h>

@interface PermissionUtils : NSObject

+(BOOL)canVisitCamera;
+(BOOL)canVisitGPS;
+(BOOL)registerNotificationPermission;

@end
