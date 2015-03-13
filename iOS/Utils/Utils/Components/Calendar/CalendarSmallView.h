#import <UIKit/UIKit.h>
#import "UIColor+expanded.h"
#import "UIUtils.h"

@protocol CalendarSmallViewDelegate;

@interface CalendarSmallView : UIView {
    
    NSDate *currentMonth;
    UILabel *labelCurrentMonth;
    BOOL isAnimating;
    BOOL prepAnimationPreviousMonth;
    BOOL prepAnimationNextMonth;
    UIImageView *animationView_A;
    UIImageView *animationView_B;
    NSArray *markedDates;
    NSArray *markedColors;
}

@property (nonatomic, assign) id <CalendarSmallViewDelegate> delegate;
@property (nonatomic, retain) NSDate *currentMonth;
@property (nonatomic, retain) UILabel *labelCurrentMonth;
@property (nonatomic, retain) UIImageView *animationView_A;
@property (nonatomic, retain) UIImageView *animationView_B;
@property (nonatomic, retain) NSArray *markedDates;
@property (nonatomic, retain) NSArray *markedColors;
@property (nonatomic, getter = calendarHeight) float calendarHeight;
@property (nonatomic, retain, getter = selectedDate) NSDate *selectedDate;
@property (assign, nonatomic) BOOL FullScreen;
@property (nonatomic, assign) BOOL mondayStyle;
@property (nonatomic, assign) CGFloat ViewDayHeight;

-(void)selectDate:(int)date;
-(void)markDates:(NSArray *)dates;
-(void)markDates:(NSArray *)dates withColors:(NSArray *)colors;

-(int)numRows;
-(void)updateSize;
-(UIImage *)drawCurrentState;
-(NSDate *)getCurrentDate;
-(NSDate *)getSelectedDate;
-(void)setCurrentData:(NSDate *)date selectedDate:(NSDate *)selectedDate;
-(id)init:(NSDate *)date;

@end

@protocol CalendarSmallViewDelegate <NSObject>

-(void)calendarView:(CalendarSmallView *)calendarView switchedToMonth:(int)month targetHeight:(float)targetHeight animated:(BOOL)animated;
-(void)calendarView:(CalendarSmallView *)calendarView dateSelected:(NSDate *)date lunarDict:(NSMutableDictionary*) dict;
-(BOOL)calendarView:(CalendarSmallView *)calendarView hasCalendarEvent:(NSInteger)year month:(NSInteger)month date:(NSInteger)date lunar:(NSString *)lunar;

@end
