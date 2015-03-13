#import "ConfigUtils.h"

@implementation ConfigUtils

+(NSString *) loadConfigString: (NSString *) key {
    return (NSString *)[self loadConfigObj:key];
}

+(int) loadConfigInt: (NSString *) key {
    return [(NSNumber *)[self loadConfigObj:key] intValue];
}

+(NSNumber *) loadConfigNumber: (NSString *) key {
    return (NSNumber *)[self loadConfigObj:key];
}

+(BOOL) loadConfigBool: (NSString *) key {
    return [(NSNumber *)[self loadConfigObj:key] boolValue];
}

+(id) loadConfigObj: (NSString *) key {
    return [[NSUserDefaults standardUserDefaults] objectForKey:key];
}

+(void) saveConfigString: (NSString *) key value: (NSString *) val {
    [self saveConfigObj:key value:val];
}

+(void) saveConfigInt: (NSString *) key value: (int) val {
    [self saveConfigObj:key value:[NSNumber numberWithInt:val]];
}

+(void) saveConfigNumber: (NSString *) key value: (NSNumber *) val {
    [self saveConfigObj:key value:val];
}

+(void) saveConfigBool: (NSString *) key value: (BOOL) val {
    [self saveConfigObj:key value:[NSNumber numberWithBool:val]];
}

+(void) saveConfigObj: (NSString *) key value: (id) val {
    NSUserDefaults * def = [NSUserDefaults standardUserDefaults];
    [def setObject:val forKey:key];
    [def synchronize];
}

@end
