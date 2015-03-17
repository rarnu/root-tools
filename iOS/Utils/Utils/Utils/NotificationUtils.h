#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@interface NotificationUtils : NSObject

+(void)startAlarm: (NSDate *)fireDate repeat:(NSCalendarUnit)repeat action:(NSString *)action body:(NSString *)body info:(NSDictionary *)info;
+(void)cancelAlarm: (UILocalNotification *)notification;
+(void)cancelAlarm: (NSDate *)date repeat:(NSCalendarUnit)repeat action:(NSString *)action;
+(void)cancelAllAlarm;

@end
