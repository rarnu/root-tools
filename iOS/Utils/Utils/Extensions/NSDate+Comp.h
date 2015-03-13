#import <Foundation/Foundation.h>

@interface NSDate (Convenience)

-(NSDate *)offsetMonth:(int)numMonths;
-(NSDate *)offsetDay:(int)numDays;
-(NSDate *)offsetHours:(int)hours;
-(NSDate *)offsetMinutes:(int)minutes;
-(int)numDaysInMonth;
-(int)firstWeekDayInMonth;
-(int)firstWeekDayInMonth:(BOOL)mondayStyle;
-(int)year;
-(int)month;
-(int)day;
-(int)hour;
-(int)minute;
-(int)week;

+(NSDate *)dateStartOfDay:(NSDate *)date;
+(NSDate *)dateStartOfWeek;
+(NSDate *)dateEndOfWeek;
+(NSDate*) convertDateFromString:(NSString*)uiDate;

@end
