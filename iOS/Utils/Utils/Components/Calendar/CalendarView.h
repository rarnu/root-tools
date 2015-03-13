#import <UIKit/UIKit.h>
#import "UIColor+expanded.h"
#import "UIUtils.h"

#define kCalendarViewTopBarHeight 20

@protocol CalendarViewDelegate;

@interface CalendarView : UIView {
    
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

@property (nonatomic, assign) id <CalendarViewDelegate> delegate;
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

-(void)selectDate:(int)date;
-(void)reset;
-(void)markDates:(NSArray *)dates;
-(void)markDates:(NSArray *)dates withColors:(NSArray *)colors;
-(void)showNextMonth;
-(void)showPreviousMonth;
-(int)numRows;
-(void)updateSize;
-(UIImage *)drawCurrentState;
-(NSDate *)getCurrentDate;
-(NSDate *)getSelectedDate;
-(void)setCurrentData:(NSDate *)date selectedDate:(NSDate *)selectedDate;

@end

@protocol CalendarViewDelegate <NSObject>

-(void)calendarView:(CalendarView *)calendarView switchedToYear:(int)year switchedToMonth:(int)month targetHeight:(float)targetHeight animated:(BOOL)animated;
-(void)calendarView:(CalendarView *)calendarView dateSelected:(NSDate *)date lunarDict:(NSMutableDictionary*) dict;
-(BOOL)calendarView:(CalendarView *)calendarView hasCalendarEvent:(NSInteger)year month:(NSInteger)month date:(NSInteger)date lunar:(NSString *)lunar;

@end
