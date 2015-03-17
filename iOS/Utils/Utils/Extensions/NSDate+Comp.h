#import <Foundation/Foundation.h>

@interface NSDate (Convenience)

-(NSDate *)offsetYears: (int)year;
-(NSDate *)offsetMonths:(int)months;
-(NSDate *)offsetDays:(int)days;
-(NSDate *)offsetHours:(int)hours;
-(NSDate *)offsetMinutes:(int)minutes;
-(NSDate *)offsetSeconds:(int)seconds;

-(int)numDaysInMonth;
-(int)firstWeekDayInMonth;
-(int)firstWeekDayInMonth:(BOOL)mondayStyle;
-(int)year;
-(int)month;
-(int)day;
-(int)hour;
-(int)minute;
-(int)second;
-(int)week;

+(NSDate *)dateStartOfDay:(NSDate *)date;
+(NSDate *)dateStartOfWeek;
+(NSDate *)dateEndOfWeek;
+(NSDate*) convertDateFromString:(NSString*)uiDate;

@end
