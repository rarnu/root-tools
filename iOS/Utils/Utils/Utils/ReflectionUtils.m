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

@end
