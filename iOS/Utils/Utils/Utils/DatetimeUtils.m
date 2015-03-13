//
//  DatetimeUtils.m
//  SchoolCalendar
//
//  Created by rarnu on 3/10/15.
//  Copyright (c) 2015 YiBan. All rights reserved.
//

#import "DatetimeUtils.h"

@implementation DatetimeUtils

+(NSArray *)getSameWeekdaysInMonth: (NSNumber *)year month:(NSNumber *)month day:(NSNumber *)day currentYear:(NSNumber *)currentYear currentMonth:(NSNumber *)currentMonth {
    
    NSCalendar * calendar = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
    NSDateComponents * comps = [[NSDateComponents alloc] init];
    [comps setCalendar:calendar];
    [comps setYear:[year integerValue]];
    [comps setMonth:[month integerValue]];
    [comps setDay:[day integerValue]];
    int weekThen = [[calendar dateFromComponents:comps] week];
    
    [comps setYear:[currentYear integerValue]];
    [comps setMonth:[currentMonth integerValue]];
    [comps setDay:1];
    NSDate * cNow = [calendar dateFromComponents:comps];
    
    NSMutableArray * listNow = [NSMutableArray array];
    int weekNow = -1;
    for (int i = 0; i < 7; i++) {
        weekNow = [cNow week];
        if (weekNow == weekThen) {
            break;
        }
        cNow = [cNow offsetDay:1];
    }
    int currentDay = [cNow day];
    while (currentDay <= 31) {
        [listNow addObject:[NSNumber numberWithInt:currentDay]];
        currentDay += 7;
    }
    return listNow;
}
+(BOOL)compareWeekday: (NSNumber *)year month:(NSNumber *)month day:(NSNumber *)day currentYear:(NSNumber *)currentYear currentMonth:(NSNumber *)currentMonth currentDay:(NSNumber *)currentDay {
    
    NSCalendar * calendar = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
    NSDateComponents * comps = [[NSDateComponents alloc] init];
    [comps setYear:[year integerValue]];
    [comps setMonth:[month integerValue]];
    [comps setDay:[day integerValue]];
    NSDate * cThen = [calendar dateFromComponents:comps];
    [comps setYear:[currentYear integerValue]];
    [comps setMonth:[currentMonth integerValue]];
    [comps setDay:[currentDay integerValue]];
    NSDate * cNow = [calendar dateFromComponents:comps];
    int weekThen = [cThen week];
    int weekNow = [cNow week];
    return weekThen == weekNow;
}
+(NSDate *)getNextWeekday: (NSNumber *)year month:(NSNumber *)month day:(NSNumber *)day {
    NSCalendar * calendar = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
    NSDateComponents * comps = [[NSDateComponents alloc] init];
    [comps setYear:[year integerValue]];
    [comps setMonth:[month integerValue]];
    [comps setDay:[day integerValue]];
    NSDate * cThen = [calendar dateFromComponents:comps];
    int weekThen = [cThen week];
    NSDate * cNow = [NSDate date];
    cNow = [cNow offsetDay:1]; // cannot be today
    int weekNow = -1;
    for (int i=0; i<7; i++) {
        weekNow = [cNow week];
        if (weekNow == weekThen) {
            break;
        }
        cNow = [cNow offsetDay:1];
    }
    return cNow;
}

@end
