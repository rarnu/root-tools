#import "NotificationUtils.h"

@implementation NotificationUtils

+(void)startAlarm: (NSDate *)fireDate repeat:(NSCalendarUnit)repeat action:(NSString *)action body:(NSString *)body info:(NSDictionary *)info {
    UIApplication * app = [UIApplication sharedApplication];
    UILocalNotification * n = [[UILocalNotification alloc] init];
    n.fireDate = fireDate;
    n.timeZone = [NSTimeZone defaultTimeZone];
    n.repeatInterval = repeat;
    n.applicationIconBadgeNumber = app.applicationIconBadgeNumber + 1;
    n.alertAction = action;
    n.alertBody = body;
    n.userInfo = info;
    [app scheduleLocalNotification:n];
}

+(void)cancelAlarm: (UILocalNotification *)notification {
    UIApplication * app = [UIApplication sharedApplication];
    [app cancelLocalNotification:notification];
}

+(void)cancelAlarm: (NSDate *)date repeat:(NSCalendarUnit)repeat action:(NSString *)action {
    UIApplication * app = [UIApplication sharedApplication];
    for (UILocalNotification * n in [app scheduledLocalNotifications]) {
        if ([n.fireDate isEqualToDate:date] && n.repeatInterval == repeat && [n.alertAction isEqualToString:action]) {
            [app cancelLocalNotification:n];
            break;
        }
    }
}

+(void)cancelAllAlarm {
    UIApplication * app = [UIApplication sharedApplication];
    [app cancelAllLocalNotifications];
}

@end
