#import "LogUtils.h"
#import "HttpUtils.h"
#import "DeviceUtils.h"

@implementation LogUtils

static NSString * LOG_URL = @"http://112.64.173.230/sdk/api/log.php";

+(void)setLogURL: (NSString *)url {
    LOG_URL = url;
}

+(void)log:(NSString *)type msg:(NSString *)msg data:(NSString *)data {
    NSDateFormatter * formatter = [[NSDateFormatter alloc] init];
    [formatter setDateFormat:@"yyyy-MM-dd HH:mm:ss"];
    NSString * currentDateStr = [formatter stringFromDate:[NSDate date]];
    DeviceInfo * dev = [DeviceUtils getDevieInfo];
    NSString * bundleId = [[NSBundle mainBundle].infoDictionary objectForKey:@"CFBundleIdentifier"];
    NSMutableDictionary * dict = [NSMutableDictionary dictionary];
    [dict setValue:bundleId forKey:@"pkg"];
    [dict setValue:type forKey:@"type"];
    [dict setValue:msg forKey:@"message"];
    [dict setValue:currentDateStr forKey:@"date"];
    [dict setValue:dev.uuid forKey:@"device"];
    if (data) {
        [dict setValue:data forKey:@"data"];
    }
    [NSThread detachNewThreadSelector:@selector(postLog:) toTarget:self withObject:dict];
    
}

+(void)postLog:(NSMutableDictionary *)dict {
    [HttpUtils postSync:LOG_URL dict:dict];
}



@end
