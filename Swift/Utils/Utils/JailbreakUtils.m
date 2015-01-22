#import "JailbreakUtils.h"

#define JAILBREAK_FILES @[@"/Application/Cydia.app",@"/Application/limera1n.app",@"/Application/greenpois0n.app",@"/Application/blackra1n.app",@"/Application/blacksn0w.app",@"/application/redsn0w.app"]

@implementation JailbreakUtils

+(BOOL) isJailbreaked {
    BOOL ret = NO;
    NSFileManager * fmgr = [NSFileManager defaultManager];
    for (NSString * file in JAILBREAK_FILES) {
        if ([fmgr fileExistsAtPath:file]) {
            ret = YES;
            break;
        }
    }
    return ret;
}


@end
