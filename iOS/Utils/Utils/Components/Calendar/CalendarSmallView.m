#import "CalendarSmallView.h"
#import <QuartzCore/QuartzCore.h>
#import "NSDate+Comp.h"
#import "NSDAte+Lunar.h"
#import "NSMutableArray+Expanded.h"
#import "UIView+Expanded.h"
#import "LunarCalendar.h"
#import "CYDatetime.h"
#import "FMDB.h"

#define NSYearCalendarUnit NSCalendarUnitYear
#define NSMonthCalendarUnit NSCalendarUnitMonth
#define NSDayCalendarUnit NSCalendarUnitDay
@implementation CalendarSmallView
@synthesize currentMonth,delegate,labelCurrentMonth, animationView_A,animationView_B;
@synthesize markedDates,markedColors,calendarHeight,selectedDate;

#define kCalendarViewTopBarHeight 0
#define kCalendarViewDayWidth [UIUtils getScreenSize].width / (9*3)
#define kCalendarViewDayHeight  ([UIUtils getScreenSize].height - [UIUtils getStatusBarHeight] - 44 - 40 ) / (13.12 * 3)
#define kCalendarViewWidth [UIUtils getScreenSize].width


#pragma mark - Select Date
-(NSString *)readyDatabase:(NSString *)dbName {
    BOOL success;
    NSFileManager *fileManager=[NSFileManager defaultManager];
    NSError *error;
    NSArray *paths=NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory=[paths objectAtIndex:0];
    NSString *writableDBPath=[documentsDirectory stringByAppendingPathComponent:dbName];
    success=[fileManager fileExistsAtPath:writableDBPath];
    if(!success) {
        NSString *defaultDBPath=[[[NSBundle mainBundle] resourcePath]stringByAppendingPathComponent:dbName];
        success=[fileManager copyItemAtPath:defaultDBPath toPath:writableDBPath error:&error];
        if(!success) {
            NSLog(@"%@",[error localizedDescription]);
        }
    }
    return writableDBPath;
}

-(NSMutableDictionary *)getChineseCalendarInfo:(NSString *)date {
    NSString *dbPath =[self readyDatabase:@"ChineseCalendar.sqlite"];
    FMDatabase *db = [FMDatabase databaseWithPath:dbPath];
    [db open];
    NSMutableDictionary *resultDict=[[NSMutableDictionary alloc] init];
    FMResultSet *rs = [db executeQuery:@"select * from ChineseCalendar where RiQi = ?", date];
    while ([rs next]) {
        [resultDict setObject:[rs stringForColumn:@"GanZhi"] forKey:@"GanZhi"];
        [resultDict setObject:[rs stringForColumn:@"Yi"] forKey:@"Yi"];
        [resultDict setObject:[rs stringForColumn:@"Ji"] forKey:@"Ji"];
        [resultDict setObject:[rs stringForColumn:@"Chong"] forKey:@"Chong"];
        [resultDict setObject:[rs stringForColumn:@"WuXing"] forKey:@"WuXing"];
    }
    [rs close];
    return resultDict;
}

-(void)selectDate:(int)date {
    NSCalendar *gregorian = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
    NSDateComponents *comps = [gregorian components:NSYearCalendarUnit | NSMonthCalendarUnit |  NSDayCalendarUnit fromDate:self.currentMonth];
    [comps setDay:date];
    if (!self.selectedDate) {
        self.selectedDate = [gregorian dateFromComponents:comps];
    }
    [self setNeedsDisplay];
    
    CYDatetime *CYDate = [[CYDatetime alloc]init];
    CYDate.year=[selectedDate year];
    CYDate.month=[selectedDate month];
    CYDate.day = [selectedDate day];
    LunarCalendar *lunarCalendar = [[CYDate convertDate] chineseCalendarDate];
    
    NSDateFormatter* formatter = [[NSDateFormatter alloc]init];
    [formatter setDateFormat:@"yyyyMMdd"];
    NSString* dateString = [[NSString alloc]initWithString:[formatter stringFromDate:self.selectedDate]];
    
    NSMutableDictionary *detailDict=[[NSMutableDictionary alloc] initWithDictionary:[self getChineseCalendarInfo:dateString]];
    [detailDict setObject:[NSString stringWithFormat:@"%d%d%d",lunarCalendar.GregorianYear,lunarCalendar.GregorianMonth,lunarCalendar.GregorianDay] forKey:@"GregorianDate"];
    
    [detailDict setObject:[NSString stringWithFormat:@"%@%@%@",lunarCalendar.IsLeap?@"闰":@"", lunarCalendar.MonthLunar,lunarCalendar.DayLunar] forKey:@"LunarDate"];
    
    [detailDict setObject:[NSString stringWithFormat:@"%@",lunarCalendar.Constellation] forKey:@"Constellation"];
    
    NSCalendar *calendar = [NSCalendar currentCalendar];

    [calendar setFirstWeekday:1];
    NSDateComponents *comps2 = [calendar components:NSCalendarUnitWeekOfMonth fromDate:self.selectedDate];
    [detailDict setObject:[NSString stringWithFormat:@"第%ld周", (long)comps2.weekOfYear] forKey:@"Weekday"];
    
    if ([delegate respondsToSelector:@selector(calendarView:dateSelected:lunarDict:)]) {
        [delegate calendarView:self dateSelected:self.selectedDate lunarDict:detailDict];
    }
}

#pragma mark - Mark Dates
-(void)markDates:(NSArray *)dates {
    
}

-(void)markDates:(NSArray *)dates withColors:(NSArray *)colors {
    self.markedDates = dates;
    self.markedColors = colors;
    
    [self setNeedsDisplay];
}

#pragma mark - Set date to now
- (void)beforeReset:(NSDate *)date {
    self.currentMonth = date;
    [self updateSize];
    [self setNeedsDisplay];
    [delegate calendarView:self switchedToMonth:[currentMonth month] targetHeight:self.calendarHeight animated:NO];
    NSCalendar *gregorian = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
    NSDateComponents *components =
    [gregorian components:(NSYearCalendarUnit | NSMonthCalendarUnit | NSDayCalendarUnit) fromDate: [NSDate date]];
    NSDate * Month = [gregorian dateFromComponents:components];
    
    if (self.currentMonth.month == Month.month && self.selectedDate.year == Month.year) {
        [self selectDate:[NSDate date].day];
    }
}

#pragma mark - update size & row count
-(void)updateSize {
    self.frameHeight = self.calendarHeight;
    [self setNeedsDisplay];
}

-(float)calendarHeight {
    return kCalendarViewTopBarHeight + [self numRows]*(kCalendarViewDayHeight+2)+1;
}

-(int)numRows {
    return 6;
}

#pragma mark - Drawing
- (void)drawRect:(CGRect)rect {
    int firstWeekDay = [self.currentMonth firstWeekDayInMonth:self.mondayStyle];
    
    NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
    [formatter setDateFormat:@"yyyy年MM月"];
    labelCurrentMonth.text = [formatter stringFromDate:self.currentMonth];
    [labelCurrentMonth sizeToFit];
    labelCurrentMonth.frameX = roundf(self.frame.size.width/2 - labelCurrentMonth.frameWidth/2);
    labelCurrentMonth.frameY = 10;
    
    [currentMonth firstWeekDayInMonth:self.mondayStyle];
    
    CGContextClearRect(UIGraphicsGetCurrentContext(),rect);
    CGContextRef context = UIGraphicsGetCurrentContext();
    
    CGRect rectangle = CGRectMake(0,0,self.frame.size.width,kCalendarViewTopBarHeight);
    CGContextAddRect(context, rectangle);
    CGContextSetFillColorWithColor(context, [UIColor whiteColor].CGColor);
    CGContextFillPath(context);
    
    NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
    dateFormatter.dateFormat=@"EEE";
    NSMutableArray *weekdays = [[NSMutableArray alloc] initWithArray:[dateFormatter shortWeekdaySymbols]];
    [weekdays moveObjectFromIndex:0 toIndex:6];
    
    NSMutableArray *chineseWeekdays = [[NSMutableArray alloc] init];
    if (self.mondayStyle) {
        NSArray *array= [NSArray arrayWithObjects:@"MON",@"TUE",@"WED",@"TUR",@"FRI",@"SAT",@"SUN",nil];
        [chineseWeekdays addObjectsFromArray:array];
    } else {
        NSArray *array= [NSArray arrayWithObjects:@"SUN",@"MON",@"TUE",@"WED",@"TUR",@"FRI",@"SAT",nil];
        [chineseWeekdays addObjectsFromArray:array];
    }
    
    for (int i =0; i<[weekdays count]; i++) {
        CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:((i==0||i==6) ? @"0xff3333" : @"0x383838")].CGColor);
        NSString *weekdayValue = (NSString *)[chineseWeekdays objectAtIndex:i];
        UIFont *font = [UIFont fontWithName:@"HelveticaNeue" size:10];
        NSMutableParagraphStyle * style = [[NSMutableParagraphStyle defaultParagraphStyle] mutableCopy];
        style.lineBreakMode = NSLineBreakByClipping;
        style.alignment = NSTextAlignmentCenter;
        [weekdayValue drawInRect:CGRectMake(i*(kCalendarViewDayWidth+2), 5, kCalendarViewDayWidth+2, 20) withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:font, NSFontAttributeName, style, NSParagraphStyleAttributeName, nil]];
    }
    
    int numRows = [self numRows];
    
    CGContextSetAllowsAntialiasing(context, NO);
    
    float gridHeight = numRows*(kCalendarViewDayHeight+2)+1;
    CGRect rectangleGrid = CGRectMake(0,kCalendarViewTopBarHeight,self.frame.size.width,gridHeight);
    CGContextAddRect(context, rectangleGrid);
    CGContextSetFillColorWithColor(context, [UIColor whiteColor].CGColor);
    CGContextFillPath(context);
    
    CGContextSetStrokeColorWithColor(context, [UIColor clearColor].CGColor);
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, 0, kCalendarViewTopBarHeight+1);
    CGContextAddLineToPoint(context, kCalendarViewWidth, kCalendarViewTopBarHeight+1);
    for (int i = 1; i<7; i++) {
        CGContextMoveToPoint(context, i*(kCalendarViewDayWidth+1)+i*1-1, kCalendarViewTopBarHeight);
        CGContextAddLineToPoint(context, i*(kCalendarViewDayWidth+1)+i*1-1, kCalendarViewTopBarHeight+gridHeight);
        
        if (i>numRows-1) {
            continue;
        }

        CGContextMoveToPoint(context, 0, kCalendarViewTopBarHeight+i*(kCalendarViewDayHeight+1)+i*1+1);
        CGContextAddLineToPoint(context, kCalendarViewWidth, kCalendarViewTopBarHeight+i*(kCalendarViewDayHeight+1)+i*1+1);
    }
    
    CGContextStrokePath(context);
    
    CGContextSetStrokeColorWithColor(context, [UIColor whiteColor].CGColor);
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, 0, kCalendarViewTopBarHeight);
    CGContextAddLineToPoint(context, kCalendarViewWidth, kCalendarViewTopBarHeight);
    for (int i = 1; i<7; i++) {
        CGContextMoveToPoint(context, i*(kCalendarViewDayWidth+1)+i*1, kCalendarViewTopBarHeight);
        CGContextAddLineToPoint(context, i*(kCalendarViewDayWidth+1)+i*1, kCalendarViewTopBarHeight+gridHeight);
        
        if (i>numRows-1) {
            continue;
        }
        CGContextMoveToPoint(context, 0, kCalendarViewTopBarHeight+i*(kCalendarViewDayHeight+1)+i*1);
        CGContextAddLineToPoint(context, kCalendarViewWidth, kCalendarViewTopBarHeight+i*(kCalendarViewDayHeight+1)+i*1);
    }
    CGContextMoveToPoint(context, 0, gridHeight+kCalendarViewTopBarHeight);
    CGContextAddLineToPoint(context, kCalendarViewWidth, gridHeight+kCalendarViewTopBarHeight);
    CGContextStrokePath(context);
    CGContextSetAllowsAntialiasing(context, YES);
    
    CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:@"0x383838"].CGColor);
    
    int numBlocks = numRows*7;
    NSDate *previousMonth = [self.currentMonth offsetMonths:-1];
    int currentMonthNumDays = [currentMonth numDaysInMonth];
    int prevMonthNumDays = [previousMonth numDaysInMonth];
    
    int selectedDateBlock = ([selectedDate day]-1)+firstWeekDay;
    
    BOOL isSelectedDatePreviousMonth = prepAnimationPreviousMonth;
    BOOL isSelectedDateNextMonth = prepAnimationNextMonth;
    
    if (self.selectedDate!=nil) {
        isSelectedDatePreviousMonth = ([selectedDate year]==[currentMonth year] && [selectedDate month]<[currentMonth month]) || [selectedDate year] < [currentMonth year];
        
        if (!isSelectedDatePreviousMonth) {
            isSelectedDateNextMonth = ([selectedDate year]==[currentMonth year] && [selectedDate month]>[currentMonth month]) || [selectedDate year] > [currentMonth year];
        }
    }
    
    if (isSelectedDatePreviousMonth) {
        int lastPositionPreviousMonth = firstWeekDay-1;
        selectedDateBlock=lastPositionPreviousMonth-([selectedDate numDaysInMonth]-[selectedDate day]);
    } else if (isSelectedDateNextMonth) {
        selectedDateBlock = [currentMonth numDaysInMonth] + (firstWeekDay-1) + [selectedDate day];
    }
    
    NSDate *todayDate = [NSDate date];
    int todayBlock = -1;
    
    if ([todayDate month] == [currentMonth month] && [todayDate year] == [currentMonth year]) {
        todayBlock = [todayDate day] + firstWeekDay - 1;
    }
    
    NSMutableParagraphStyle * style = [[NSMutableParagraphStyle defaultParagraphStyle] mutableCopy];
    style.lineBreakMode = NSLineBreakByClipping;
    style.alignment = NSTextAlignmentCenter;
    NSString * colorString = @"";
    
    for (int i=0; i<numBlocks; i++) {
        int targetDate ;
        int targetColumn = i%7;
        int targetRow = i/7;
        int targetX = targetColumn * (kCalendarViewDayWidth+2);
        int targetY = kCalendarViewTopBarHeight + targetRow * (kCalendarViewDayHeight+2);
        
        CYDatetime *CYDate = [[CYDatetime alloc]init];
        
        if (i<firstWeekDay) {
            targetDate = (prevMonthNumDays-firstWeekDay)+(i+1);
            colorString = (targetColumn==0||targetColumn==6) ? @"0xFFE4E1" : @"0xDCDCDC";
            CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            
            if([self.currentMonth month]==1) {
                CYDate.year=[self.currentMonth year]-1;
                CYDate.month =12;
            } else {
                CYDate.year=[self.currentMonth year];
                CYDate.month =[self.currentMonth month]-1;
            }
            
        } else if (i>=(firstWeekDay+currentMonthNumDays)) {
            targetDate = (i+1) - (firstWeekDay+currentMonthNumDays);
            colorString = (targetColumn==0||targetColumn==6) ? @"0xFFE4E1" : @"0xDCDCDC";
            CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            if([self.currentMonth month]==12) {
                CYDate.year=[self.currentMonth year]+1;
                CYDate.month =1;
            } else {
                CYDate.year=[self.currentMonth year];
                CYDate.month =[self.currentMonth month]+1;
            }
        } else {
            targetDate = (i-firstWeekDay)+1;
            if (self.mondayStyle) {
                colorString = (targetColumn==5||targetColumn==6) ? @"0xff3333" : @"0x383838";
                CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            } else {
                colorString = (targetColumn==0||targetColumn==6) ? @"0xff3333" : @"0x383838";
                CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            }
            CYDate.year=[self.currentMonth year];
            CYDate.month =[self.currentMonth month];
        }
        
        CYDate.day = targetDate;
        
        NSString *date = [NSString stringWithFormat:@"%i",targetDate];
        
        if (selectedDate && i==selectedDateBlock) {
            CGRect rectangleGrid = CGRectMake(targetX,targetY,kCalendarViewDayWidth+2,kCalendarViewDayHeight+2);
            CGContextAddRect(context, rectangleGrid);
            colorString = @"0xF08080";
            CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            CGContextFillPath(context);
            colorString = @"0xffffff";
            CGContextSetFillColorWithColor(context, [UIColor whiteColor].CGColor);
        } else if (todayBlock==i) {
            CGRect rectangleGrid = CGRectMake(targetX,targetY,kCalendarViewDayWidth+2,kCalendarViewDayHeight+2);
            CGContextAddRect(context, rectangleGrid);
            colorString = @"0xF08080";
            CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            CGContextFillPath(context);
            colorString = @"0xffffff";
            CGContextSetFillColorWithColor(context, [UIColor whiteColor].CGColor);
        }
        
        [date drawInRect:CGRectMake(targetX, targetY, kCalendarViewDayWidth, kCalendarViewDayHeight) withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[UIFont fontWithName:@"HelveticaNeue" size:11], NSFontAttributeName, style, NSParagraphStyleAttributeName, [UIColor colorWithHexString:colorString], NSForegroundColorAttributeName, nil]];
        
    }
    
    if (!self.markedDates || isSelectedDatePreviousMonth || isSelectedDateNextMonth) {
        return;
    }
    
    for (int i = 0; i<[self.markedDates count]; i++) {
        id markedDateObj = [self.markedDates objectAtIndex:i];
        
        int targetDate;
        if ([markedDateObj isKindOfClass:[NSNumber class]]) {
            targetDate = [(NSNumber *)markedDateObj intValue];
        } else if ([markedDateObj isKindOfClass:[NSDate class]]) {
            NSDate *date = (NSDate *)markedDateObj;
            targetDate = [date day];
        } else {
            continue;
        }
        
        int targetBlock = firstWeekDay + (targetDate-1);
        int targetColumn = targetBlock%7;
        int targetRow = targetBlock/7;
        
        int targetX = targetColumn * (kCalendarViewDayWidth+2) + 7;
        int targetY = kCalendarViewTopBarHeight + targetRow * (kCalendarViewDayHeight+2) + 38;
        
        CGRect rectangle = CGRectMake(targetX+10,targetY+4,10,4);
        CGContextAddRect(context, rectangle);
        
        UIColor *color;
        if (selectedDate && selectedDateBlock==targetBlock) {
            color = [UIColor whiteColor];
        }  else if (todayBlock==targetBlock) {
            color = [UIColor whiteColor];
        } else {
            color  = (UIColor *)[markedColors objectAtIndex:i];
        }
        
        CGContextSetFillColorWithColor(context, color.CGColor);
        CGContextFillPath(context);
    }
    
}

#pragma mark - Draw image for animation
-(UIImage *)drawCurrentState {
    float targetHeight = kCalendarViewTopBarHeight + [self numRows]*(kCalendarViewDayHeight+2)+1;
    
    UIGraphicsBeginImageContext(CGSizeMake(kCalendarViewWidth, targetHeight-kCalendarViewTopBarHeight));
    CGContextRef c = UIGraphicsGetCurrentContext();
    CGContextTranslateCTM(c, 0, -kCalendarViewTopBarHeight);
    [self.layer renderInContext:c];
    UIImage* viewImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return viewImage;
}

-(NSDate *)getCurrentDate {
    return currentMonth;
}

-(NSDate *)getSelectedDate {
    return selectedDate;
}

-(void)setCurrentData:(NSDate *)date selectedDate:(NSDate *)selected {
    currentMonth = date;
    selectedDate = selected;
}

#pragma mark - Init
-(id)init:(NSDate *)date {
    self = [super initWithFrame:CGRectMake(0, 0, kCalendarViewWidth, 0)];
    if (self) {
        self.contentMode = UIViewContentModeTop;
        self.clipsToBounds=YES;
        isAnimating=NO;
        self.ViewDayHeight = 7 * kCalendarViewDayHeight;
        [self performSelector:@selector(beforeReset:) withObject:date afterDelay:0.1];
        
    }
    return self;
}

-(void)dealloc {
    
    self.delegate=nil;
    self.currentMonth=nil;
    self.labelCurrentMonth=nil;
    self.markedDates=nil;
    self.markedColors=nil;
    
}
@end
