#import "LunarCalendar.h"

#define NSYearCalendarUnit NSCalendarUnitYear
#define NSMonthCalendarUnit NSCalendarUnitMonth
#define NSDayCalendarUnit NSCalendarUnitDay
#define NSGregorianCalendar NSCalendarIdentifierGregorian

int LunarCalendarInfo[] = {
    0x04bd8,0x04ae0,0x0a570,0x054d5,0x0d260,0x0d950,0x16554,0x056a0,0x09ad0,0x055d2,
	0x04ae0,0x0a5b6,0x0a4d0,0x0d250,0x1d255,0x0b540,0x0d6a0,0x0ada2,0x095b0,0x14977,
	0x04970,0x0a4b0,0x0b4b5,0x06a50,0x06d40,0x1ab54,0x02b60,0x09570,0x052f2,0x04970,
	0x06566,0x0d4a0,0x0ea50,0x06e95,0x05ad0,0x02b60,0x186e3,0x092e0,0x1c8d7,0x0c950,
	0x0d4a0,0x1d8a6,0x0b550,0x056a0,0x1a5b4,0x025d0,0x092d0,0x0d2b2,0x0a950,0x0b557,
    
	0x06ca0,0x0b550,0x15355,0x04da0,0x0a5b0,0x14573,0x052b0,0x0a9a8,0x0e950,0x06aa0,
	0x0aea6,0x0ab50,0x04b60,0x0aae4,0x0a570,0x05260,0x0f263,0x0d950,0x05b57,0x056a0,
	0x096d0,0x04dd5,0x04ad0,0x0a4d0,0x0d4d4,0x0d250,0x0d558,0x0b540,0x0b6a0,0x195a6,
	0x095b0,0x049b0,0x0a974,0x0a4b0,0x0b27a,0x06a50,0x06d40,0x0af46,0x0ab60,0x09570,
	0x04af5,0x04970,0x064b0,0x074a3,0x0ea50,0x06b58,0x055c0,0x0ab60,0x096d5,0x092e0,
    
	0x0c960,0x0d954,0x0d4a0,0x0da50,0x07552,0x056a0,0x0abb7,0x025d0,0x092d0,0x0cab5,
	0x0a950,0x0b4a0,0x0baa4,0x0ad50,0x055d9,0x04ba0,0x0a5b0,0x15176,0x052b0,0x0a930,
	0x07954,0x06aa0,0x0ad50,0x05b52,0x04b60,0x0a6e6,0x0a4e0,0x0d260,0x0ea65,0x0d530,
	0x05aa0,0x076a3,0x096d0,0x04bd7,0x04ad0,0x0a4d0,0x1d0b6,0x0d250,0x0d520,0x0dd45,
	0x0b5a0,0x056d0,0x055b2,0x049b0,0x0a577,0x0a4b0,0x0aa50,0x1b255,0x06d20,0x0ada0,
    
    0x04b63,0x0937f,0x049f8,0x04970,0x064b0,0x068a6,0x0ea5f,0x06b20,0x0a6c4,0x0aaef,
    0x092e0,0x0d2e3,0x0c960,0x0d557,0x0d4a0,0x0da50,0x05d55,0x056a0,0x0a6d0,0x055d4,
    0x052d0,0x0a9b8,0x0a950,0x0b4a0,0x0b6a6,0x0ad50,0x055a0,0x0aba4,0x0a5b0,0x052b0,
    0x0b273,0x06930,0x07337,0x06aa0,0x0ad50,0x04b55,0x04b6f,0x0a570,0x054e4,0x0d260,
    0x0e968,0x0d520,0x0daa0,0x06aa6,0x056df,0x04ae0,0x0a9d4,0x0a4d0,0x0d150,0x0f252,
    0x0d520};

@implementation LunarCalendar

-(id)init {
    self = [super init];
    
	HeavenlyStems = [NSArray arrayWithObjects:@"甲",@"乙",@"丙",@"丁",@"戊",@"己",@"庚",@"辛",@"壬",@"癸",nil];
	EarthlyBranches = [NSArray arrayWithObjects:@"子",@"丑",@"寅",@"卯",@"辰",@"巳",@"午",@"未",@"申",@"酉",@"戌",@"亥",nil];
	LunarZodiac = [NSArray arrayWithObjects:@"鼠",@"牛",@"虎",@"兔",@"龙",@"蛇",@"马",@"羊",@"猴",@"鸡",@"狗",@"猪",nil];
	
	SolarTerms = [NSArray arrayWithObjects:@"立春", @"雨水", @"惊蛰", @"春分", @"清明", @"谷雨", @"立夏", @"小满", @"芒种", @"夏至", @"小暑", @"大暑", @"立秋", @"处暑", @"白露", @"秋分", @"寒露", @"霜降", @"立冬", @"小雪", @"大雪", @"冬至", @"小寒", @"大寒", nil];
	
	arrayMonth = [NSArray arrayWithObjects:@"正月", @"二月", @"三月", @"四月", @"五月", @"六月", @"七月", @"八月", @"九月",  @"十月", @"冬月", @"腊月", nil];
	
	arrayDay = [NSArray arrayWithObjects:@"初一", @"初二", @"初三", @"初四", @"初五", @"初六", @"初七", @"初八", @"初九", @"初十", @"十一", @"十二", @"十三", @"十四", @"十五", @"十六", @"十七", @"十八", @"十九", @"二十", @"廿一", @"廿二", @"廿三", @"廿四", @"廿五", @"廿六", @"廿七", @"廿八", @"廿九", @"三十", @"三一", nil];
    
    holiday=[[NSMutableArray alloc] init];
    
    if(!self) {
        return nil;
    }
	
	return self;
}

-(void)loadWithDate:(NSDate *)adate {
    if (adate == nil) {
        [self loadWithDate:[NSDate date]];
    } else {
		HeavenlyStems = [NSArray arrayWithObjects:@"甲",@"乙",@"丙",@"丁",@"戊",@"己",@"庚",@"辛",@"壬",@"癸",nil];
        EarthlyBranches = [NSArray arrayWithObjects:@"子",@"丑",@"寅",@"卯",@"辰",@"巳",@"午",@"未",@"申",@"酉",@"戌",@"亥",nil];
        LunarZodiac = [NSArray arrayWithObjects:@"鼠",@"牛",@"虎",@"兔",@"龙",@"蛇",@"马",@"羊",@"猴",@"鸡",@"狗",@"猪",nil];
        
        SolarTerms = [NSArray arrayWithObjects:@"立春", @"雨水", @"惊蛰", @"春分", @"清明", @"谷雨", @"立夏", @"小满", @"芒种", @"夏至", @"小暑", @"大暑", @"立秋", @"处暑", @"白露", @"秋分", @"寒露", @"霜降", @"立冬", @"小雪", @"大雪", @"冬至", @"小寒", @"大寒", nil];
        
        arrayMonth = [NSArray arrayWithObjects:@"正月", @"二月", @"三月", @"四月", @"五月", @"六月", @"七月", @"八月", @"九月",  @"十月", @"冬月", @"腊月", nil];
        
        arrayDay = [NSArray arrayWithObjects:@"初一", @"初二", @"初三", @"初四", @"初五", @"初六", @"初七", @"初八", @"初九", @"初十", @"十一", @"十二", @"十三", @"十四", @"十五", @"十六", @"十七", @"十八", @"十九", @"二十", @"廿一", @"廿二", @"廿三", @"廿四", @"廿五", @"廿六", @"廿七", @"廿八", @"廿九", @"三十", @"三一", nil];
		
		NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
		
		[dateFormatter setDateStyle:NSDateFormatterMediumStyle];
		[dateFormatter setTimeStyle:NSDateFormatterNoStyle];
		
		[dateFormatter setDateFormat:@"yyyy"];
		year = [[dateFormatter stringFromDate:adate] intValue];
		
		[dateFormatter setDateFormat:@"MM"];
		month = [[dateFormatter stringFromDate:adate] intValue];
		
		[dateFormatter setDateFormat:@"dd"];
		day = [[dateFormatter stringFromDate:adate] intValue];
						
		thisdate = adate;
	}
}

-(void)InitializeValue {
	NSString *start = @"1900-01-31";
	NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
	[dateFormatter setDateFormat:@"yyyy-MM-dd"];
	NSString *end = [dateFormatter stringFromDate:thisdate];
	
	NSDateFormatter *f = [[NSDateFormatter alloc] init];
	[f setDateFormat:@"yyyy-MM-dd"];
	NSDate *startDate = [f dateFromString:start];
	NSDate *endDate = [f dateFromString:end];
	
	NSCalendar *gregorianCalendar = [[NSCalendar alloc] initWithCalendarIdentifier:NSGregorianCalendar];
	NSDateComponents *components = [gregorianCalendar components:NSDayCalendarUnit fromDate:startDate toDate:endDate options:0];
	
	int dayCyclical=(int)(([components day] + 30)/(86400/(3600*24)))+10;

	int sumdays = (int)[components day];
		
	int tempdays = 0;

	//计算农历年
	for (lunarYear = 1900; lunarYear < 2100 && sumdays > 0; lunarYear++) {
		tempdays = [self LunarYearDays:lunarYear];
		sumdays -= tempdays;
	}
	
	if (sumdays < 0) {
		sumdays += tempdays;
		lunarYear--;
	}
	
	//计算闰月
	doubleMonth = [self DoubleMonth:lunarYear];
	isLeap = false;
	
	//计算农历月
	for (lunarMonth = 1; lunarMonth < 13 && sumdays > 0; lunarMonth++) {
		//闰月
		if (doubleMonth > 0 && lunarMonth == (doubleMonth + 1) && isLeap == false) {
			--lunarMonth;
			isLeap = true;
			tempdays = [self DoubleMonthDays:lunarYear];
		} else {
			tempdays = [self MonthDays:lunarYear:lunarMonth];
		}
		
		//解除闰月
		if (isLeap == true && lunarMonth == (doubleMonth + 1)) {
			isLeap = false;
		}
		sumdays -= tempdays;
	}
	
	//计算农历日
	if (sumdays == 0 && doubleMonth > 0 && lunarMonth == doubleMonth + 1) {
		if (isLeap) {
			isLeap = false;
		} else {
			isLeap = true;
			--lunarMonth;
		}
	}
	
	if (sumdays < 0) {
		sumdays += tempdays;
		--lunarMonth;
	}
	
	lunarDay = sumdays + 1;
	
	//计算节气
	[self ComputeSolarTerm];
	
	solarTermTitle = @"";
	for (int i=0; i<2; i++) {
		NSDateFormatter *currentFormatter = [[NSDateFormatter alloc] init];
		[currentFormatter setDateFormat:@"yyyyMMdd"];
        if (solarTerm[i].solarDate == [[currentFormatter stringFromDate:thisdate] intValue]) {
			solarTermTitle = solarTerm[i].solarName;
        }
	}

	monthLunar = (NSString *)[arrayMonth objectAtIndex:(lunarMonth - 1)];
	dayLunar = (NSString *)[arrayDay objectAtIndex:(lunarDay - 1)];
    
    NSString *chineseHoliday= [self getChineseHoliday:lunarMonth day:lunarDay];
    if(chineseHoliday!=nil){
        [holiday addObject:chineseHoliday];
    }
    
    NSString *normalHoliday=[self getWorldHoliday:month day:day];
    if (normalHoliday!=nil) {
        [holiday addObject:normalHoliday];
    }
    
    NSString *weekHoliday=[self getWeekHoliday:year month:month day:day];
    if (weekHoliday!=nil) {
        [holiday addObject:weekHoliday];
    }
    
	zodiacLunar = (NSString *)[LunarZodiac objectAtIndex:((lunarYear - 4) % 60 % 12)];
	
	yearHeavenlyStem = (NSString *)[HeavenlyStems objectAtIndex:((lunarYear - 4) % 60 % 10)];
    if ((((year-1900)*12+month+13)%10) == 0) {
		monthHeavenlyStem = (NSString *)[HeavenlyStems objectAtIndex:9];
    } else {
		monthHeavenlyStem = (NSString *)[HeavenlyStems objectAtIndex:(((year-1900)*12+month+13)%10-1)];
    }
	dayHeavenlyStem = (NSString *)[HeavenlyStems objectAtIndex:(dayCyclical%10)];
	
	yearEarthlyBranch = (NSString *)[EarthlyBranches objectAtIndex:((lunarYear - 4) % 60 % 12)];
    if ((((year-1900)*12+month+13)%12) == 0) {
		monthEarthlyBranch = (NSString *)[EarthlyBranches objectAtIndex:11];
    } else {
		monthEarthlyBranch = (NSString *)[EarthlyBranches objectAtIndex:(((year-1900)*12+month+13)%12-1)];
    }
	dayEarthlyBranch = (NSString *)[EarthlyBranches objectAtIndex:(dayCyclical%12)];
}

-(NSString *)getChineseHoliday:(int)aMonth day:(int)aDay {
    NSDictionary *chineseHolidayDict = [NSDictionary dictionaryWithObjectsAndKeys:
                                        @"春节", @"1|1",
                                        @"元宵", @"1|15",
                                        @"端午", @"5|5",
                                        @"七夕", @"7|7",
                                        @"中元", @"7|15",
                                        @"中秋", @"8|15",
                                        @"重阳", @"9|9",
                                        @"腊八", @"12|8",
                                        @"小年", @"12|24",
                                        @"除夕", @"12|30",
                                        nil];
    
    return [chineseHolidayDict objectForKey:[NSString stringWithFormat:@"%d|%d",aMonth,aDay]];
}

-(NSString *)getWorldHoliday:(int)aMonth day:(int)aDay {
    NSString *monthDay;
    if(aMonth<10 && aDay<10) {
        monthDay=[NSString stringWithFormat:@"0%i0%i",aMonth,aDay] ;
    } else if(aMonth<10 && aDay>9) {
        monthDay=[NSString stringWithFormat:@"0%i%i",aMonth,aDay] ;
    } else if(aMonth>9 && aDay<10) {
        monthDay=[NSString stringWithFormat:@"%i0%i",aMonth,aDay] ;
    } else {
        monthDay=[NSString stringWithFormat:@"%i%i",aMonth,aDay] ;
    }
    
    NSMutableDictionary *dict=[[NSMutableDictionary alloc] init];
    [dict setObject:@"元旦" forKey:@"0101"];
    [dict setObject:@"中国第13亿人口日" forKey:@"0106"];
    [dict setObject:@"周恩来逝世纪念日" forKey:@"0108"];
    [dict setObject:@"释迦如来成道日" forKey:@"0115"];
    [dict setObject:@"列宁逝世纪念日 国际声援南非日 弥勒佛诞辰" forKey:@"0121"];
    [dict setObject:@"世界湿地日" forKey:@"0202"];
    [dict setObject:@"二七大罢工纪念日" forKey:@"0207"];
    [dict setObject:@"国际气象节" forKey:@"0210"];
    [dict setObject:@"情人节" forKey:@"0214"];
    [dict setObject:@"中国12亿人口日" forKey:@"0215"];
    [dict setObject:@"邓小平逝世纪念日" forKey:@"0219"];
    [dict setObject:@"国际母语日 反对殖民制度斗争日" forKey:@"0221"];
    [dict setObject:@"苗族芦笙节" forKey:@"0222"];
    [dict setObject:@"第三世界青年日" forKey:@"0224"];
    [dict setObject:@"世界居住条件调查日" forKey:@"0228"];
    [dict setObject:@"国际海豹日" forKey:@"0301"];
    [dict setObject:@"全国爱耳日" forKey:@"0303"];
    [dict setObject:@"学雷锋纪念日 中国青年志愿者服务日" forKey:@"0305"];
    [dict setObject:@"妇女节" forKey:@"0308"];
    [dict setObject:@"保护母亲河日" forKey:@"0309"];
    [dict setObject:@"国际尊严尊敬日" forKey:@"0311"];
    [dict setObject:@"国际警察日 白色情人节" forKey:@"0314"];
    [dict setObject:@"消费者权益日" forKey:@"0315"];
    [dict setObject:@"手拉手情系贫困小伙伴全国统一行动日" forKey:@"0316"];
    [dict setObject:@"中国国医节 国际航海日 爱尔兰圣帕特里克节" forKey:@"0317"];
    [dict setObject:@"全国科技人才活动日" forKey:@"0318"];
    [dict setObject:@"世界森林日 消除种族歧视国际日 世界儿歌日 世界睡眠日" forKey:@"0321"];
    [dict setObject:@"世界水日" forKey:@"0322"];
    [dict setObject:@"世界气象日" forKey:@"0323"];
    [dict setObject:@"世界防治结核病日" forKey:@"0324"];
    [dict setObject:@"全国中小学生安全教育日" forKey:@"0325"];
    [dict setObject:@"中国黄花岗七十二烈士殉难纪念" forKey:@"0329"];
    [dict setObject:@"巴勒斯坦国土日" forKey:@"0330"];
    [dict setObject:@"愚人节 全国爱国卫生运动月 税收宣传月" forKey:@"0401"];
    [dict setObject:@"国际儿童图书日" forKey:@"0402"];
    [dict setObject:@"世界卫生日" forKey:@"0407"];
    [dict setObject:@"世界帕金森病日" forKey:@"0411"];
    [dict setObject:@"全国企业家活动日" forKey:@"0421"];
    [dict setObject:@"世界地球日 世界法律日" forKey:@"0422"];
    [dict setObject:@"世界图书和版权日" forKey:@"0423"];
    [dict setObject:@"亚非新闻工作者日 世界青年反对殖民主义日" forKey:@"0424"];
    [dict setObject:@"全国预防接种宣传日" forKey:@"0425"];
    [dict setObject:@"世界知识产权日" forKey:@"0426"];
    [dict setObject:@"世界交通安全反思日" forKey:@"0430"];
    [dict setObject:@"国际劳动节" forKey:@"0501"];
    [dict setObject:@"世界哮喘日 世界新闻自由日" forKey:@"0503"];
    [dict setObject:@"中国五四青年节 科技传播日" forKey:@"0504"];
    [dict setObject:@"碘缺乏病防治日 日本男孩节" forKey:@"0505"];
    [dict setObject:@"世界红十字日" forKey:@"0508"];
    [dict setObject:@"国际护士节" forKey:@"0512"];
    [dict setObject:@"国际家庭日" forKey:@"0515"];
    [dict setObject:@"国际电信日" forKey:@"0517"];
    [dict setObject:@"国际博物馆日" forKey:@"0518"];
    [dict setObject:@"全国学生营养日 全国母乳喂养宣传日" forKey:@"0520"];
    [dict setObject:@"国际牛奶日" forKey:@"0523"];
    [dict setObject:@"世界向人体条件挑战日" forKey:@"0526"];
    [dict setObject:@"中国“五卅”运动纪念日" forKey:@"0530"];
    [dict setObject:@"世界无烟日 英国银行休假日" forKey:@"0531"];
    [dict setObject:@"国际儿童节" forKey:@"0601"];
    [dict setObject:@"世界环境保护日" forKey:@"0605"];
    [dict setObject:@"世界献血者日" forKey:@"0614"];
    [dict setObject:@"防治荒漠化和干旱日" forKey:@"0617"];
    [dict setObject:@"世界难民日" forKey:@"0620"];
    [dict setObject:@"中国儿童慈善活动日" forKey:@"0622"];
    [dict setObject:@"国际奥林匹克日" forKey:@"0623"];
    [dict setObject:@"全国土地日" forKey:@"0625"];
    [dict setObject:@"国际禁毒日 国际宪章日 禁止药物滥用和非法贩运国际日 支援酷刑受害者国际日" forKey:@"0626"];
    [dict setObject:@"世界青年联欢节" forKey:@"0630"];
    [dict setObject:@"建党节 香港回归纪念日 中共诞辰 世界建筑日" forKey:@"0701"];
    [dict setObject:@"国际体育记者日" forKey:@"0702"];
    [dict setObject:@"朱德逝世纪念日" forKey:@"0706"];
    [dict setObject:@"抗日战争纪念日" forKey:@"0707"];
    [dict setObject:@"世界人口日 中国航海日" forKey:@"0711"];
    [dict setObject:@"世界语创立日" forKey:@"0726"];
    [dict setObject:@"第一次世界大战爆发" forKey:@"0728"];
    [dict setObject:@"非洲妇女日" forKey:@"0730"];
    [dict setObject:@"建军节" forKey:@"0801"];
    [dict setObject:@"恩格斯逝世纪念日" forKey:@"0805"];
    [dict setObject:@"国际电影节" forKey:@"0806"];
    [dict setObject:@"中国男子节" forKey:@"0808"];
    [dict setObject:@"国际青年节" forKey:@"0812"];
    [dict setObject:@"国际左撇子日" forKey:@"0813"];
    [dict setObject:@"抗日战争胜利纪念" forKey:@"0815"];
    [dict setObject:@"全国律师咨询日" forKey:@"0826"];
    [dict setObject:@"日本签署无条件投降书日" forKey:@"0902"];
    [dict setObject:@"中国抗日战争胜利纪念日" forKey:@"0903"];
    [dict setObject:@"瑞士萨永中世纪节" forKey:@"0905"];
    [dict setObject:@"帕瓦罗蒂去世" forKey:@"0906"];
    [dict setObject:@"国际扫盲日 国际新闻工作者日" forKey:@"0908"];
    [dict setObject:@"毛泽东逝世纪念日" forKey:@"0909"];
    [dict setObject:@"中国教师节 世界预防自杀日" forKey:@"0910"];
    [dict setObject:@"世界清洁地球日" forKey:@"0914"];
    [dict setObject:@"国际臭氧层保护日 中国脑健康日" forKey:@"0916"];
    [dict setObject:@"九·一八事变纪念日" forKey:@"0918"];
    [dict setObject:@"国际爱牙日" forKey:@"0920"];
    [dict setObject:@"世界停火日 预防世界老年性痴呆宣传日" forKey:@"0921"];
    [dict setObject:@"世界旅游日" forKey:@"0927"];
    [dict setObject:@"孔子诞辰" forKey:@"0928"];
    [dict setObject:@"国际翻译日" forKey:@"0930"];
    [dict setObject:@"国庆节 世界音乐日 国际老人节" forKey:@"1001"];
    [dict setObject:@"国际和平与民主自由斗争日" forKey:@"1002"];
    [dict setObject:@"世界动物日" forKey:@"1004"];
    [dict setObject:@"国际教师节" forKey:@"1005"];
    [dict setObject:@"中国老年节" forKey:@"1006"];
    [dict setObject:@"全国高血压日 世界视觉日" forKey:@"1008"];
    [dict setObject:@"世界邮政日 万国邮联日" forKey:@"1009"];
    [dict setObject:@"辛亥革命纪念日 世界精神卫生日 世界居室卫生日" forKey:@"1010"];
    [dict setObject:@"世界保健日 国际教师节 中国少年先锋队诞辰日 世界保健日" forKey:@"1013"];
    [dict setObject:@"世界标准日" forKey:@"1014"];
    [dict setObject:@"国际盲人节(白手杖节)" forKey:@"1015"];
    [dict setObject:@"世界粮食日" forKey:@"1016"];
    [dict setObject:@"世界消除贫困日" forKey:@"1017"];
    [dict setObject:@"世界骨质疏松日" forKey:@"1020"];
    [dict setObject:@"世界传统医药日" forKey:@"1022"];
    [dict setObject:@"联合国日 世界发展新闻日" forKey:@"1024"];
    [dict setObject:@"中国男性健康日" forKey:@"1028"];
    [dict setObject:@"万圣节 世界勤俭日" forKey:@"1031"];
    [dict setObject:@"达摩祖师圣诞" forKey:@"1102"];
    [dict setObject:@"柴科夫斯基逝世悼念日" forKey:@"1106"];
    [dict setObject:@"十月社会主义革命纪念日" forKey:@"1107"];
    [dict setObject:@"中国记者日" forKey:@"1108"];
    [dict setObject:@"全国消防安全宣传教育日" forKey:@"1109"];
    [dict setObject:@"世界青年节" forKey:@"1110"];
    [dict setObject:@"光棍节 国际科学与和平周" forKey:@"1111"];
    [dict setObject:@"孙中山诞辰纪念日" forKey:@"1112"];
    [dict setObject:@"世界糖尿病日" forKey:@"1114"];
    [dict setObject:@"泰国大象节" forKey:@"1115"];
    [dict setObject:@"国际大学生节 世界学生节 世界戒烟日" forKey:@"1117"];
    [dict setObject:@"世界儿童日" forKey:@"1120"];
    [dict setObject:@"世界问候日 世界电视日" forKey:@"1121"];
    [dict setObject:@"国际声援巴勒斯坦人民国际日" forKey:@"1129"];
    [dict setObject:@"世界艾滋病日" forKey:@"1201"];
    [dict setObject:@"废除一切形式奴役世界日" forKey:@"1202"];
    [dict setObject:@"世界残疾人日" forKey:@"1203"];
    [dict setObject:@"全国法制宣传日" forKey:@"1204"];
    [dict setObject:@"国际经济和社会发展志愿人员日 世界弱能人士日" forKey:@"1205"];
    [dict setObject:@"国际民航日" forKey:@"1207"];
    [dict setObject:@"国际儿童电视日" forKey:@"1208"];
    [dict setObject:@"世界足球日 一二·九运动纪念日" forKey:@"1209"];
    [dict setObject:@"世界人权日" forKey:@"1210"];
    [dict setObject:@"世界防止哮喘日" forKey:@"1211"];
    [dict setObject:@"西安事变纪念日" forKey:@"1212"];
    [dict setObject:@"南京大屠杀纪念日" forKey:@"1213"];
    [dict setObject:@"国际儿童广播电视节" forKey:@"1214"];
    [dict setObject:@"世界强化免疫日" forKey:@"1215"];
    [dict setObject:@"澳门回归纪念" forKey:@"1220"];
    [dict setObject:@"国际篮球日" forKey:@"1221"];
    [dict setObject:@"平安夜" forKey:@"1224"];
    [dict setObject:@"圣诞节" forKey:@"1225"];
    [dict setObject:@"毛泽东诞辰纪念日 节礼日" forKey:@"1226"];
    [dict setObject:@"国际生物多样性日" forKey:@"1229"];
    
    return [dict objectForKey:monthDay];
}

-(NSString *)getWeekHoliday:(int)aYear month:(int)aMonth day:(int)aDay {
    
    NSMutableDictionary *dict=[[NSMutableDictionary alloc] init];
    [dict setObject:@"世界哮喘日" forKey:@"0512"];
    [dict setObject:@"国际母亲节 救助贫困母亲日" forKey:@"0520"];
    [dict setObject:@"全国助残日" forKey:@"0530"];
    [dict setObject:@"国际牛奶日" forKey:@"0532"];
    [dict setObject:@"中国文化遗产日" forKey:@"0626"];
    [dict setObject:@"国际父亲节" forKey:@"0630"];
    [dict setObject:@"国际合作节" forKey:@"0716"];
    [dict setObject:@"被奴役国家周" forKey:@"0730"];
    [dict setObject:@"国际和平日" forKey:@"0932"];
    [dict setObject:@"全民国防教育日" forKey:@"0936"];
    [dict setObject:@"国际聋人节 世界儿童日" forKey:@"0940"];
    [dict setObject:@"世界海事日 世界心脏病日" forKey:@"0950"];
    [dict setObject:@"国际住房日 世界建筑日 世界人居日" forKey:@"1011"];
    [dict setObject:@"国际减灾日" forKey:@"1023"];
    [dict setObject:@"世界视觉日" forKey:@"1024"];
    [dict setObject:@"感恩节" forKey:@"1144"];
    [dict setObject:@"国际儿童电视广播日" forKey:@"1220"];
    
    NSString *result=nil;
    for (id key in [dict allKeys]) {
        NSString *dictMonth=[key substringToIndex:2];
        int dictWeek=[[[key substringFromIndex:2] substringToIndex:1] intValue];
        int dictDayInWeek=[[[key substringFromIndex:3] substringToIndex:1] intValue];
        
        if(aMonth==[dictMonth intValue]) {
            NSString *resultDay=[self getWeekDay:aYear month:aMonth week:dictWeek dayInWeek:dictDayInWeek];
            if(resultDay) {
                if([resultDay intValue]==aDay ) {
                    result=[dict objectForKey:key];
                }
            }
        }
    }

    return result;
}

-(NSString *)getWeekDay:(int)aYear month:(int)aMonth week:(int)aWeek dayInWeek:(int)aDay {
    NSString* dateStr =[NSString stringWithFormat:@"%i 1 %i +0000",aMonth,aYear];
    NSDateFormatter* formater = [[NSDateFormatter alloc] init];
    [formater setDateFormat:@"M d yyyy zzzz"];
    NSDate* date = [formater dateFromString:dateStr];

    NSDateComponents *components = [[NSCalendar currentCalendar] components: NSCalendarUnitWeekday | NSDayCalendarUnit | NSMonthCalendarUnit | NSYearCalendarUnit fromDate:date];
    int firstWeek=(int)[components weekday]-1;
    int result=0;
    aDay=aDay+1;
    
    if(aWeek<5) {
        result=(firstWeek>aDay?7:0)+7*(aWeek-1)+aDay-firstWeek;
    }
    
    if(result==0) {
        return nil;
    } else {
        return [NSString stringWithFormat:@"%i",result];
    }
}

-(int)LunarYearDays:(int)y {
	int i, sum = 348;
	for (i = 0x8000; i > 0x8; i >>= 1) {
		if ((LunarCalendarInfo[y - 1900] & i) != 0)
			sum += 1;
	}
	return (sum + [self DoubleMonthDays:y]);
}

-(int)DoubleMonth:(int)y {
	return (LunarCalendarInfo[y - 1900] & 0xf);
}

///返回农历年闰月的天数
-(int)DoubleMonthDays:(int)y {
    if ([self DoubleMonth:y] != 0) {
		return (((LunarCalendarInfo[y - 1900] & 0x10000) != 0) ? 30 : 29);
    } else {
		return (0);
    }
}

///返回农历年月份的总天数
-(int)MonthDays:(int)y :(int)m {
	return (((LunarCalendarInfo[y - 1900] & (0x10000 >> m)) != 0) ? 30 : 29);
}

-(void)ComputeSolarTerm {
	for (int n = month * 2 - 1; n <= month * 2; n++) {
		double Termdays = [self Term:year:n:YES];
		double mdays = [self AntiDayDifference:year:floor(Termdays)];
		int hour = (int)floor((double)[self Tail:Termdays] * 24);
		int minute = (int)floor((double)([self Tail:Termdays] * 24 - hour) * 60);
		int tMonth = (int)ceil((double)n / 2);
		int tday = (int)mdays % 100;
		
        if (n >= 3) {
			solarTerm[n - month * 2 + 1].solarName = [SolarTerms objectAtIndex:(n - 3)];
        } else {
			solarTerm[n - month * 2 + 1].solarName = [SolarTerms objectAtIndex:(n + 21)];
        }
		NSDateComponents *components = [[NSDateComponents alloc] init];
		[components setYear:year];
		[components setMonth:tMonth]; 
		[components setDay:tday];
		[components setHour:hour];
		[components setMinute:minute];

		NSCalendar *gregorian = [[NSCalendar alloc] initWithCalendarIdentifier:NSGregorianCalendar];
		NSDate *ldate = [gregorian dateFromComponents:components];
		NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
		
		[dateFormatter setDateFormat:@"yyyyMMdd"];
				
		solarTerm[n - month * 2 + 1].solarDate = [[dateFormatter stringFromDate:ldate] intValue];
	}
}

-(double)Tail:(double)x {
	return x - floor(x);
}

-(double)Term:(int)y :(int)n :(bool)pd {
	//儒略日
	double juD = y * (365.2423112 - 6.4e-14 * (y - 100) * (y - 100) - 3.047e-8 * (y - 100)) + 15.218427 * n + 1721050.71301;
	
	//角度
	double tht = 3e-4 * y - 0.372781384 - 0.2617913325 * n;
	
	//年差实均数
	double yrD = (1.945 * sin(tht) - 0.01206 * sin(2 * tht)) * (1.048994 - 2.583e-5 * y);
	
	//朔差实均数
	double shuoD = -18e-4 * sin(2.313908653 * y - 0.439822951 - 3.0443 * n);
	
	double vs = (pd) ? (juD + yrD + shuoD - [self EquivalentStandardDay:y:1:0] - 1721425) : (juD - [self EquivalentStandardDay:y:1:0] - 1721425);
	return vs;
}

-(double)AntiDayDifference:(int)y :(double)x {
	int m = 1;
	for (int j = 1; j <= 12; j++) {
		int mL = [self DayDifference:y:(j+1):1] - [self DayDifference:y:j:1];
		if (x <= mL || j == 12) {
			m = j;
			break;
        } else {
			x -= mL;
        }
	}
	return 100 * m + x;
}

-(double)EquivalentStandardDay:(int)y :(int)m :(int)d {
	//Julian的等效标准天数
	double v = (y - 1) * 365 + floor((double)((y - 1) / 4)) + [self DayDifference:y:m:d] - 2;
	
	if (y > 1582) {//Gregorian的等效标准天数
		v += -floor((double)((y - 1) / 100)) + floor((double)((y - 1) / 400)) + 2; 
	} 
	return v;
}

-(int)DayDifference:(int)y :(int)m :(int)d {
	int ifG = [self IfGregorian:y:m:d:1];
	//NSArray *monL = [NSArray arrayWithObjects:, nil];
	NSInteger monL[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
	if (ifG == 1) {
        if ((y % 100 != 0 && y % 4 == 0) || (y % 400 == 0)) {
			monL[2] += 1;
        } else {
            if (y % 4 == 0) {
				monL[2] += 1;
            }
		}
	}

	int v = 0;
	
    for (int i = 0; i <= m - 1; i++) {
		v += monL[i];
    }

	v += d;
	if (y == 1582) {
        if (ifG == 1) {
			v -= 10;
        }
        if (ifG == -1) {
			v = 0;  //infinity
        }
	}
	return v;
}

-(int)IfGregorian:(int)y :(int)m :(int)d :(int)opt {
	if (opt == 1) {
        if (y > 1582 || (y == 1582 && m > 10) || (y == 1582 && m == 10 && d > 14)) {
			return (1);	 //Gregorian
        } else {
            if (y == 1582 && m == 10 && d >= 5 && d <= 14) {
				return (-1);  //空
            } else {
				return (0);  //Julian
            }
        }
	}
	
    if (opt == 2) {
		return (1);	 //Gregorian
    }
    if (opt == 3) {
		return (0);	 //Julian
    }
	return (-1);
}

-(NSString *)MonthLunar {
	return monthLunar;
}

-(NSString *)DayLunar {
	return dayLunar;
}

-(NSString *)ZodiacLunar {
	return zodiacLunar;
}

-(NSString *)YearHeavenlyStem {
	return yearHeavenlyStem;
}

-(NSString *)MonthHeavenlyStem {
	return monthHeavenlyStem;
}

-(NSString *)DayHeavenlyStem {
	return dayHeavenlyStem;
}

-(NSString *)YearEarthlyBranch {
	return yearEarthlyBranch;
}

-(NSString *)MonthEarthlyBranch {
	return monthEarthlyBranch;
}

-(NSString *)DayEarthlyBranch {
	return dayEarthlyBranch;
}

-(NSString *)SolarTermTitle {
	return solarTermTitle;
}

-(NSMutableArray *)Holiday {
	return holiday;
}

-(bool)IsLeap {
	return isLeap;
}

-(int)GregorianYear {
	NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
	[formatter setDateFormat:@"yyyy"];
	int ret = [[formatter stringFromDate:thisdate] intValue];

	return ret;
}

-(int)GregorianMonth {
	NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
	[formatter setDateFormat:@"MM"];
	int ret = [[formatter stringFromDate:thisdate] intValue];

	return ret;
}

-(int)GregorianDay {
	NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
	[formatter setDateFormat:@"dd"];
	int ret = [[formatter stringFromDate:thisdate] intValue];
	return ret;
}

-(int)Weekday {
	NSCalendar* cal = [NSCalendar currentCalendar];
	NSDateComponents* weekday = [cal components:NSCalendarUnitWeekday fromDate:thisdate];
	return (int)[weekday weekday];
}

//计算星座
-(NSString *)Constellation {
	NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
	[formatter setDateFormat:@"MMdd"];
	int intConstellation = [[formatter stringFromDate:thisdate] intValue];
	
	if (intConstellation >= 120 && intConstellation <= 218)
		return @"水瓶座";
	else if (intConstellation >= 219 && intConstellation <= 320)
		return @"双鱼座";
	else if (intConstellation >= 321 && intConstellation <= 420)
		return @"白羊座";
	else if (intConstellation >= 421 && intConstellation <= 520)
		return @"金牛座";
	else if (intConstellation >= 521 && intConstellation <= 621)
		return @"双子座";
	else if (intConstellation >= 622 && intConstellation <= 722)
		return @"巨蟹座";
	else if (intConstellation >= 723 && intConstellation <= 822)
		return @"狮子座";
	else if (intConstellation >= 823 && intConstellation <= 922)
		return @"处女座";
	else if (intConstellation >= 923 && intConstellation <= 1022)
		return @"天秤座";
	else if (intConstellation >= 1023 && intConstellation <= 1121)
		return @"天蝎座";
	else if (intConstellation >= 1122 && intConstellation <= 1221)
		return @"射手座";
	else
		return @"摩羯座";
}

@end

