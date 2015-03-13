#import "CalendarView.h"
#import <QuartzCore/QuartzCore.h>
#import "NSDate+Comp.h"
#import "NSDate+Lunar.h"
#import "NSMutableArray+Expanded.h"
#import "UIView+Expanded.h"
#import "LunarCalendar.h"
#import "CYDatetime.h"
#import "FMDB.h"

#define NSYearCalendarUnit NSCalendarUnitYear
#define NSMonthCalendarUnit NSCalendarUnitMonth
#define NSDayCalendarUnit NSCalendarUnitDay

@implementation CalendarView

@synthesize currentMonth,delegate,labelCurrentMonth, animationView_A,animationView_B;
@synthesize markedDates,markedColors,calendarHeight,selectedDate;

#define kCalendarViewWidth [UIUtils getScreenSize].width
#define kCalendarViewDayWidth [UIUtils getScreenSize].width / 7.21
#define kCalendarViewDayHeight  ((self.FullScreen)?([UIUtils getScreenSize].height - [UIUtils getStatusBarHeight] - 44 - 50 - kCalendarViewTopBarHeight) / 6:(([UIUtils getScreenSize].height - [UIUtils getStatusBarHeight] - 44 - 40) / 2 - kCalendarViewTopBarHeight - 40) / 6)

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
        [fileManager copyItemAtPath:defaultDBPath toPath:writableDBPath error:&error];
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
-(void)reset {
    NSCalendar *gregorian = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
    NSDateComponents *components =
    [gregorian components:(NSYearCalendarUnit | NSMonthCalendarUnit | NSDayCalendarUnit) fromDate: [NSDate date]];
    if (!self.currentMonth) {
        self.currentMonth = [gregorian dateFromComponents:components]; //clean month
    }
    
    [self updateSize];
    [self setNeedsDisplay];
    [delegate calendarView:self switchedToYear:[currentMonth year] switchedToMonth:[currentMonth month] targetHeight:self.calendarHeight animated:NO];
    
    [self selectDate:[NSDate date].day];
}

#pragma mark - Next & Previous
-(void)showNextMonth {
    
    if (isAnimating) return;
    self.markedDates=nil;
    isAnimating=YES;
    prepAnimationNextMonth=YES;
    
    [self setNeedsDisplay];
    
    int lastBlock = [currentMonth firstWeekDayInMonth:self.mondayStyle]+[currentMonth numDaysInMonth]-1;
    int numBlocks = [self numRows]*7;
    BOOL hasNextMonthDays = lastBlock<numBlocks;
    
    float oldSize = self.calendarHeight;
    UIImage *imageCurrentMonth = [self drawCurrentState];
    
    self.currentMonth = [currentMonth offsetMonth:1];
    if ([delegate respondsToSelector:@selector(calendarView:switchedToYear:switchedToMonth:targetHeight:animated:)]) [delegate calendarView:self switchedToYear:[currentMonth year] switchedToMonth:[currentMonth month] targetHeight:self.calendarHeight animated:YES];
    prepAnimationNextMonth=NO;
    [self setNeedsDisplay];
    
    UIImage *imageNextMonth = [self drawCurrentState];
    float targetSize = fmaxf(oldSize, self.calendarHeight);
    UIView *animationHolder = [[UIView alloc] initWithFrame:CGRectMake(0, kCalendarViewTopBarHeight, kCalendarViewWidth, targetSize-kCalendarViewTopBarHeight)];
    [animationHolder setClipsToBounds:YES];
    [self addSubview:animationHolder];
    
    animationView_A = [[UIImageView alloc] initWithImage:imageCurrentMonth];
    animationView_B = [[UIImageView alloc] initWithImage:imageNextMonth];
    [animationHolder addSubview:animationView_A];
    [animationHolder addSubview:animationView_B];
    
    if (hasNextMonthDays) {
        animationView_B.frameY = animationView_A.frameY + animationView_A.frameHeight - (kCalendarViewDayHeight+3);
    } else {
        animationView_B.frameY = animationView_A.frameY + animationView_A.frameHeight -3;
    }
    
    __block CalendarView *blockSafeSelf = self;
    [UIView animateWithDuration:.35 animations:^{
        [self updateSize];
        if (hasNextMonthDays) {
            animationView_A.frameY = -animationView_A.frameHeight + kCalendarViewDayHeight+3;
        } else {
            animationView_A.frameY = -animationView_A.frameHeight + 3;
        }
        animationView_B.frameY = 0;
    } completion:^(BOOL finished) {
        [animationView_A removeFromSuperview];
        [animationView_B removeFromSuperview];
        blockSafeSelf.animationView_A=nil;
        blockSafeSelf.animationView_B=nil;
        isAnimating=NO;
        [animationHolder removeFromSuperview];
    }];
}


-(void)showPreviousMonth {
    if (isAnimating) return;
    isAnimating=YES;
    self.markedDates=nil;
    prepAnimationPreviousMonth = YES;
    [self setNeedsDisplay];
    BOOL hasPreviousDays = [currentMonth firstWeekDayInMonth:self.mondayStyle]>1;
    float oldSize = self.calendarHeight;
    UIImage *imageCurrentMonth = [self drawCurrentState];
    
    self.currentMonth = [currentMonth offsetMonth:-1];
    if ([delegate respondsToSelector:@selector(calendarView:switchedToYear:switchedToMonth:targetHeight:animated:)]) {
        [delegate calendarView:self switchedToYear:[currentMonth year]  switchedToMonth:[currentMonth month] targetHeight:self.calendarHeight animated:YES];
    }
    prepAnimationPreviousMonth=NO;
    [self setNeedsDisplay];
    UIImage *imagePreviousMonth = [self drawCurrentState];
    
    float targetSize = fmaxf(oldSize, self.calendarHeight);
    UIView *animationHolder = [[UIView alloc] initWithFrame:CGRectMake(0, kCalendarViewTopBarHeight, kCalendarViewWidth, targetSize-kCalendarViewTopBarHeight)];
    
    [animationHolder setClipsToBounds:YES];
    [self addSubview:animationHolder];
    
    animationView_A = [[UIImageView alloc] initWithImage:imageCurrentMonth];
    animationView_B = [[UIImageView alloc] initWithImage:imagePreviousMonth];
    [animationHolder addSubview:animationView_A];
    [animationHolder addSubview:animationView_B];
    
    if (hasPreviousDays) {
        animationView_B.frameY = animationView_A.frameY - (animationView_B.frameHeight-kCalendarViewDayHeight) + 3;
    } else {
        animationView_B.frameY = animationView_A.frameY - animationView_B.frameHeight + 3;
    }
    
    __block CalendarView *blockSafeSelf = self;
    [UIView animateWithDuration:.35 animations:^{
        [self updateSize];
        if (hasPreviousDays) {
            animationView_A.frameY = animationView_B.frameHeight-(kCalendarViewDayHeight+3);
        } else {
            animationView_A.frameY = animationView_B.frameHeight-3;
        }
        animationView_B.frameY = 0;
    } completion:^(BOOL finished) {
        [animationView_A removeFromSuperview];
        [animationView_B removeFromSuperview];
        blockSafeSelf.animationView_A=nil;
        blockSafeSelf.animationView_B=nil;
        isAnimating=NO;
        [animationHolder removeFromSuperview];
    }];
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

#pragma mark - Touches
-(void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    UITouch *touch = [touches anyObject];
    CGPoint touchPoint = [touch locationInView:self];
    
    self.selectedDate=nil;
    
    if (touchPoint.y > kCalendarViewTopBarHeight) {
        float xLocation = touchPoint.x;
        float yLocation = touchPoint.y-kCalendarViewTopBarHeight;
        
        int column = floorf(xLocation/(kCalendarViewDayWidth+2));
        int row = floorf(yLocation/(kCalendarViewDayHeight+2));
        
        int blockNr = (column+1)+row*7;
        
        int firstWeekDay = [self.currentMonth firstWeekDayInMonth:self.mondayStyle];
        int date = blockNr-firstWeekDay;
        [self selectDate:date];
        return;
    }
    
    self.markedDates=nil;
    self.markedColors=nil;
    
    CGRect rectArrowLeft = CGRectMake(0, 0, 50, 40);
    CGRect rectArrowRight = CGRectMake(self.frame.size.width-50, 0, 50, 40);
    
    if (CGRectContainsPoint(rectArrowLeft, touchPoint)) {

    } else if (CGRectContainsPoint(rectArrowRight, touchPoint)) {

    } else if (CGRectContainsPoint(self.labelCurrentMonth.frame, touchPoint)) {

        int currentMonthIndex = [self.currentMonth month];
        int todayMonth = [[NSDate date] month];
        [self reset];
        if ((todayMonth!=currentMonthIndex) && [delegate respondsToSelector:@selector(calendarView:switchedToYear:switchedToMonth:targetHeight:animated:)]) {
            [delegate calendarView:self switchedToYear:[currentMonth year]  switchedToMonth:[currentMonth month] targetHeight:self.calendarHeight animated:NO];
        }
    }
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
    CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:@"0xf3f3f3"].CGColor);
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
    
    CGContextSetStrokeColorWithColor(context, [UIColor colorWithHexString:@"0xF3F3F3"].CGColor);
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
    NSDate *previousMonth = [self.currentMonth offsetMonth:-1];
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
            colorString = (targetColumn==0||targetColumn==6) ? @"0xf39999" : @"aaaaaa";
            CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            
            if([self.currentMonth month]==1) {
                CYDate.year=[self.currentMonth year]-1;
                CYDate.month =12;
            } else {
                CYDate.year=[self.currentMonth year];
                CYDate.month =[self.currentMonth month]-1;
            }
            
        } else if (i>=(firstWeekDay+currentMonthNumDays)) { //next month
            targetDate = (i+1) - (firstWeekDay+currentMonthNumDays);
            colorString = (targetColumn==0||targetColumn==6) ? @"0xf39999" : @"aaaaaa";
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
        LunarCalendar *lunarCalendar = [[CYDate convertDate] chineseCalendarDate];
        NSString * lunarDate = [lunarCalendar.DayLunar isEqualToString:@"初一"] ? lunarCalendar.MonthLunar : [[NSString alloc]initWithFormat:@"%@",[lunarCalendar.SolarTermTitle isEqualToString:@""] ? lunarCalendar.DayLunar : lunarCalendar.SolarTermTitle];
        
        NSString *date = [NSString stringWithFormat:@"%i",targetDate];
        
        if (selectedDate && i==selectedDateBlock) {
            CGRect rectangleGrid = CGRectMake(targetX,targetY,kCalendarViewDayWidth+2,kCalendarViewDayHeight+2);
            CGContextAddRect(context, rectangleGrid);
            colorString = @"0x006dbc";
            CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            CGContextFillPath(context);
            colorString = @"0xffffff";
            CGContextSetFillColorWithColor(context, [UIColor whiteColor].CGColor);
        } else if (todayBlock==i) {
            CGRect rectangleGrid = CGRectMake(targetX,targetY,kCalendarViewDayWidth+2,kCalendarViewDayHeight+2);
            CGContextAddRect(context, rectangleGrid);
            colorString = @"0x383838";
            CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:colorString].CGColor);
            CGContextFillPath(context);
            colorString = @"0xffffff";
            CGContextSetFillColorWithColor(context, [UIColor whiteColor].CGColor);
        }
        
        if (self.FullScreen) {
            NSString *holiday=[lunarCalendar.Holiday count]>0 ? [[[lunarCalendar.Holiday objectAtIndex:0] componentsSeparatedByString:@" "] objectAtIndex:0] : @"";
            if (holiday.length > 4) {
                holiday = @"";
            }
            
            [holiday drawInRect:CGRectMake(targetX, targetY+50, kCalendarViewDayWidth, kCalendarViewDayHeight) withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[UIFont fontWithName:@"HelveticaNeue" size:12], NSFontAttributeName, style, NSParagraphStyleAttributeName, [UIColor colorWithHexString:colorString], NSForegroundColorAttributeName, nil]];
            
            [date drawInRect:CGRectMake(targetX, targetY + 5, kCalendarViewDayWidth, kCalendarViewDayHeight) withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[UIFont fontWithName:@"Courier" size:18], NSFontAttributeName, style, NSParagraphStyleAttributeName, [UIColor colorWithHexString:colorString], NSForegroundColorAttributeName, nil]];
            
            [lunarDate drawInRect:CGRectMake(targetX, targetY + 25, kCalendarViewDayWidth, kCalendarViewDayHeight) withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[UIFont fontWithName:@"HelveticaNeue" size:12], NSFontAttributeName, style, NSParagraphStyleAttributeName, [UIColor colorWithHexString:colorString], NSForegroundColorAttributeName, nil]];
            
        } else {
            [date drawInRect:CGRectMake(targetX, targetY + 5, kCalendarViewDayWidth, kCalendarViewDayHeight) withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[UIFont fontWithName:@"Georgia" size:16], NSFontAttributeName, style, NSParagraphStyleAttributeName, [UIColor colorWithHexString:colorString], NSForegroundColorAttributeName, nil]];
        }
        
        if ([self.delegate respondsToSelector:@selector(calendarView:hasCalendarEvent:month:date:lunar:)]) {
            if ([self.delegate calendarView:self hasCalendarEvent:CYDate.year month:CYDate.month date:CYDate.day lunar:lunarDate]) {
                CGContextSetFillColorWithColor(context, [UIColor colorWithHexString:@"0x00A5ED"].CGColor);
                if (self.FullScreen) {
                    CGContextFillRect(context, CGRectMake(targetX + 16, targetY + 40, kCalendarViewDayWidth-32, 3));
                } else {
                    CGContextFillRect(context, CGRectMake(targetX + 16, targetY + 35, kCalendarViewDayWidth-32, 3));
                }
            }
        }
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
    CGContextTranslateCTM(c, 0, -kCalendarViewTopBarHeight);    // <-- shift everything up by 40px when drawing.
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
-(id)init {
    self = [super initWithFrame:CGRectMake(0, 0, kCalendarViewWidth, 0)];
    if (self) {
        self.contentMode = UIViewContentModeTop;
        self.clipsToBounds=YES;
        
        isAnimating=NO;
        labelCurrentMonth = [[UILabel alloc] initWithFrame:CGRectMake(34, 0, kCalendarViewWidth-68, 40)];
        labelCurrentMonth.backgroundColor=[UIColor whiteColor];
        labelCurrentMonth.font = [UIFont fontWithName:@"HelveticaNeue-Bold" size:17];
        labelCurrentMonth.textColor = [UIColor colorWithHexString:@"0x383838"];
        labelCurrentMonth.textAlignment = NSTextAlignmentCenter;
        
        [self performSelector:@selector(reset) withObject:nil afterDelay:0.1];
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
