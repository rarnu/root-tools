#import <Foundation/Foundation.h>

struct SolarTerm
{
	__unsafe_unretained NSString *solarName;
	int solarDate;
};

@interface LunarCalendar : NSObject
{	
	NSArray *HeavenlyStems;//天干表
	NSArray *EarthlyBranches;//地支表
	NSArray *LunarZodiac;//生肖表
	NSArray *SolarTerms;//24节气表
	NSArray *arrayMonth;//农历月表
	NSArray *arrayDay;//农历天表

	NSDate *thisdate;
	
	int year;//年
	int month;//月
	int day;//日
	
	int lunarYear;	//农历年
	int lunarMonth;	//农历月
	int doubleMonth;	//闰月
	bool isLeap;	  //是否闰月标记
	int lunarDay;	//农历日
	
	struct SolarTerm solarTerm[2];
	
	NSString *yearHeavenlyStem;//年天干
	NSString *monthHeavenlyStem;//月天干
	NSString *dayHeavenlyStem;//日天干
	
	NSString *yearEarthlyBranch;//年地支
	NSString *monthEarthlyBranch;//月地支
	NSString *dayEarthlyBranch;//日地支
	
	NSString *monthLunar;//农历月
	NSString *dayLunar;//农历日
	
	NSString *zodiacLunar;//生肖
	
	NSString *solarTermTitle; //24节气
    
    //added by cyrusleung
    NSMutableArray *holiday;//节日
}

-(void)loadWithDate:(NSDate *)date;//加载数据

-(void)InitializeValue;//添加数据
-(int)LunarYearDays:(int)y;
-(int)DoubleMonth:(int)y;
-(int)DoubleMonthDays:(int)y;
-(int)MonthDays:(int)y :(int)m;
-(void)ComputeSolarTerm;

-(double)Term:(int)y :(int)n :(bool)pd;
-(double)AntiDayDifference:(int)y :(double)x;
-(double)EquivalentStandardDay:(int)y :(int)m :(int)d;
-(int)IfGregorian:(int)y :(int)m :(int)d :(int)opt;
-(int)DayDifference:(int)y :(int)m :(int)d;
-(double)Tail:(double)x;

-(NSString *)MonthLunar;//农历
-(NSString *)DayLunar;//农历日
-(NSString *)ZodiacLunar;//年生肖
-(NSString *)YearHeavenlyStem;//年天干
-(NSString *)MonthHeavenlyStem;//月天干
-(NSString *)DayHeavenlyStem;//日天干
-(NSString *)YearEarthlyBranch;//年地支
-(NSString *)MonthEarthlyBranch;//月地支
-(NSString *)DayEarthlyBranch;//日地支
-(NSString *)SolarTermTitle;//节气
-(NSMutableArray *)Holiday;//节日
-(bool)IsLeap;//是不是农历闰年？？
-(int)GregorianYear;//阳历年
-(int)GregorianMonth;//阳历月
-(int)GregorianDay;//阳历天
-(int)Weekday;//一周的第几天
-(NSString *)Constellation;//星座

@end

