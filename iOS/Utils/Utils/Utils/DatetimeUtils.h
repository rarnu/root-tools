//
//  DatetimeUtils.h
//  SchoolCalendar
//
//  Created by rarnu on 3/10/15.
//  Copyright (c) 2015 YiBan. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NSDate+Comp.h"


@interface DatetimeUtils : NSObject


+(NSDate *)dateFromTimeInMillis:(NSTimeInterval)millis;
+(NSTimeInterval)dateToTimeInMillis:(NSDate *)date;
+(NSArray *)getSameWeekdaysInMonth: (NSNumber *)year month:(NSNumber *)month day:(NSNumber *)day currentYear:(NSNumber *)currentYear currentMonth:(NSNumber *)currentMonth;
+(BOOL)compareWeekday: (NSNumber *)year month:(NSNumber *)month day:(NSNumber *)day currentYear:(NSNumber *)currentYear currentMonth:(NSNumber *)currentMonth currentDay:(NSNumber *)currentDay;
+(NSDate *)getNextWeekday: (NSNumber *)year month:(NSNumber *)month day:(NSNumber *)day;

@end
