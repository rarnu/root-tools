#import "JsonUtils.h"
#import <objc/runtime.h>

@implementation JsonUtils

+(void)fillJsonToObject:(id)obj jsonString:(NSString *)jsonString {
    NSData * data = [jsonString dataUsingEncoding:NSUTF8StringEncoding];
    [self fillJsonToObject:obj jsonData:data];
}

+(void)fillJsonToObject:(id)obj jsonData:(NSData *)jsonData {
    NSJSONSerialization * json = [NSJSONSerialization JSONObjectWithData:jsonData options:NSJSONReadingMutableLeaves error:nil];
    [self fillJsonToObject:obj json:json];
}

+(void)fillJsonToObject:(id)obj json:(NSJSONSerialization *)json {
    NSMutableArray * fields = [self getClassFields:obj];
    NSMutableString * jsonKey;
    for (NSString * field in fields) {
        jsonKey = [NSMutableString stringWithString:field];
        if ([jsonKey hasPrefix:@"_"]) {
            [jsonKey deleteCharactersInRange:NSMakeRange(0, 1)];
        }
        [obj setValue:[json valueForKey:jsonKey] forKey:field];
    }
}

+(NSString *)objectToJsonString: (id)obj {
    NSMutableString * str = [[NSMutableString alloc] initWithString:@"{"];
    NSMutableArray * fields = [self getClassFields:obj];
    for (NSString * field in fields) {
        id val = [obj valueForKey:field];
        if (val != nil) {
            if ([val isKindOfClass:[NSString class]]) {
                [str appendFormat:@"\"%@\":\"%@\",", field, val];
            } else {
                [str appendFormat:@"\"%@\":%@,", field, val];
            }
        }
    }
    [str deleteCharactersInRange:NSMakeRange(str.length - 1, 1)];
    [str appendString:@"}"];
    return str;
}

+(NSMutableArray *)getClassFields: (id)obj {
    NSMutableArray * array = [NSMutableArray array];
    Class clz = [obj class];
    unsigned int ivarsCnt = 0;
    Ivar *ivars = class_copyIvarList(clz, &ivarsCnt);
    for (const Ivar *p = ivars; p < ivars + ivarsCnt; ++p) {
        Ivar const ivar = *p;
        NSMutableString * key = [NSMutableString stringWithUTF8String:ivar_getName(ivar)];
        // remove start _
        if ([key hasPrefix:@"_"]) {
            [key deleteCharactersInRange:NSMakeRange(0, 1)];
        }
        [array addObject:key];
    }
    return array;
}

@end
