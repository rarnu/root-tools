#import "EmulatorUtils.h"
#import <sys/sysctl.h>

@implementation EmulatorUtils

+(NSString * )platform {
    size_t size;
    sysctlbyname("hw.machine", NULL, &size, NULL, 0);
    char *machine = malloc(size);
    sysctlbyname("hw.machine", machine, &size, NULL, 0);
    NSString* platform = [NSString stringWithCString:machine encoding:NSUTF8StringEncoding];
    free(machine);
    return platform;
}

+(BOOL)isEmulator {
    return [[self platform] isEqualToString:@"i386"];
}

@end
