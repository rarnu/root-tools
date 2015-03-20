#import <Foundation/Foundation.h>
#define TYPE_APP_OPEN @"app_open"
#define TYPE_APP_CLOSE @"app_close"
#define TYPE_APP_VIEW_OPEN @"app_view_open"
#define TYPE_APP_VIEW_CLOSE @"app_view_close"
#define TYPE_APP_DATABASE_OPEN @"app_database_open"
#define TYPE_APP_DATABASE_CLOSE @"app_database_close"
#define TYPE_APP_CRASH @"app_crash"
#define TYPE_APP_NETWORK @"app_network"
#define TYPE_APP_DEVICE @"app_device"
#define TYPE_APP_SCHEMA @"app_schema"
#define TYPE_APP_CUSTOM @"app_custom"

@interface LogUtils : NSObject

+(void)setLogURL: (NSString *)url;
+(void)log:(NSString *)type msg:(NSString *)msg data:(NSString *)data;

@end
