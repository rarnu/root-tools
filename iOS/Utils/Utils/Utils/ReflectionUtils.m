#import "ReflectionUtils.h"
#import <objc/runtime.h>

@implementation ReflectionUtils

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

+(NSMutableArray *)getClassMethods: (id)obj {
    NSMutableArray * array = [NSMutableArray array];
    unsigned int imethodCnt = 0;
    Class clz = [obj class];
    Method * methods = class_copyMethodList(clz, &imethodCnt);
    for (int i = 0; i < imethodCnt; i++) {
        Method method = methods[i];
        SEL selector = method_getName(method);
        NSString *name = NSStringFromSelector(selector);
        [array addObject:name];
    }
    return array;
}

+(id)invokeClassMethod:(id)obj method:(NSString *)method param:(id)param {
    id ret = nil;
    @try {
        SEL selMethod = NSSelectorFromString(method);
        #pragma clang diagnostic ignored "-Warc-performSelector-leaks"
        ret = [obj performSelector:selMethod withObject:param];
    }
    @catch (NSException *exception) {
        
    }
    return ret;
    
}

+(id)invokeClassMethod:(id)obj method:(NSString *)method param1:(id)param1 param2:(id)param2 {
    id ret = nil;
    @try {
        SEL selMethod = NSSelectorFromString(method);
        #pragma clang diagnostic ignored "-Warc-performSelector-leaks"
        ret = [obj performSelector:selMethod withObject:param1 withObject:param2];
    }
    @catch (NSException *exception) {
        
    }
    return ret;
}


@end
