#import "NSDate+Lunar.h"

@implementation NSDate (LunarCalendar)

- (LunarCalendar *)chineseCalendarDate {
    LunarCalendar *lunarCalendar = [[LunarCalendar alloc] init];
    [lunarCalendar loadWithDate:self];
    [lunarCalendar InitializeValue];
    return lunarCalendar;
}

@end