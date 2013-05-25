package com.zoe.calendar.component;

import java.util.Locale;

import android.content.Context;

import com.zoe.calendar.R;

public class LunarCalendar {
	private static String[] lunarCalendarNumber = null;

	private static String[] lunarCalendarTen = null;

	private static String[] year_of_birth = null;
	
	private static String[] lunarTerm = null;

	private static String lunarLeapTag = null, lunarMonthTag = null, zhengyueTag = null;
	
	Context mContext;
	
	public int lunarYear = 0;
	
	public int lunarMonth = 0;
	
	public int lunarDay = 0;
	
	public int solarYear = 0;
	
	public int solarMonth = 0;
	
	public int solarDay = 0;
	
	public boolean  isLeapMonth = false;
	
	public double D_solar_terms = 0.2422;
	
	public LunarCalendar(Context context){
		mContext = context;
		init();
	}
	
	private void init() {
		if (lunarCalendarNumber == null) {
			lunarCalendarNumber = new String[12];
			lunarCalendarNumber[0] = getString(R.string.chineseNumber1);
			lunarCalendarNumber[1] = getString(R.string.chineseNumber2);
			lunarCalendarNumber[2] = getString(R.string.chineseNumber3);
			lunarCalendarNumber[3] = getString(R.string.chineseNumber4);
			lunarCalendarNumber[4] = getString(R.string.chineseNumber5);
			lunarCalendarNumber[5] = getString(R.string.chineseNumber6);
			lunarCalendarNumber[6] = getString(R.string.chineseNumber7);
			lunarCalendarNumber[7] = getString(R.string.chineseNumber8);
			lunarCalendarNumber[8] = getString(R.string.chineseNumber9);
			lunarCalendarNumber[9] = getString(R.string.chineseNumber10);
			lunarCalendarNumber[10] = getString(R.string.chineseNumber11);
			lunarCalendarNumber[11] = getString(R.string.chineseNumber12);
		}

		if (lunarCalendarTen == null) {
			lunarCalendarTen = new String[5];
			lunarCalendarTen[0] = getString(R.string.chineseTen0);
			lunarCalendarTen[1] = getString(R.string.chineseTen1);
			lunarCalendarTen[2] = getString(R.string.chineseTen2);
			lunarCalendarTen[3] = getString(R.string.chineseTen3);
			lunarCalendarTen[4] = getString(R.string.chineseTen4);
		}

		if (year_of_birth == null) {
			year_of_birth = new String[12];
			year_of_birth[0] = getString(R.string.animals0);
			year_of_birth[1] = getString(R.string.animals1);
			year_of_birth[2] = getString(R.string.animals2);
			year_of_birth[3] = getString(R.string.animals3);
			year_of_birth[4] = getString(R.string.animals4);
			year_of_birth[5] = getString(R.string.animals5);
			year_of_birth[6] = getString(R.string.animals6);
			year_of_birth[7] = getString(R.string.animals7);
			year_of_birth[8] = getString(R.string.animals8);
			year_of_birth[9] = getString(R.string.animals9);
			year_of_birth[10] = getString(R.string.animals10);
			year_of_birth[11] = getString(R.string.animals11);
		}

		if (lunarLeapTag == null)
			lunarLeapTag = getString(R.string.leap_month);
		if (lunarMonthTag == null)
			lunarMonthTag = getString(R.string.month);
		if (zhengyueTag == null)
			zhengyueTag = getString(R.string.zheng);

		if (lunarTerm == null) {
			lunarTerm = new String[24];
			lunarTerm[0] = getString(R.string.terms0);
			lunarTerm[1] = getString(R.string.terms1);
			lunarTerm[2] = getString(R.string.terms2);
			lunarTerm[3] = getString(R.string.terms3);
			lunarTerm[4] = getString(R.string.terms4);
			lunarTerm[5] = getString(R.string.terms5);
			lunarTerm[6] = getString(R.string.terms6);
			lunarTerm[7] = getString(R.string.terms7);
			lunarTerm[8] = getString(R.string.terms8);
			lunarTerm[9] = getString(R.string.terms9);
			lunarTerm[10] = getString(R.string.terms10);
			lunarTerm[11] = getString(R.string.terms11);
			lunarTerm[12] = getString(R.string.terms12);
			lunarTerm[13] = getString(R.string.terms13);
			lunarTerm[14] = getString(R.string.terms14);
			lunarTerm[15] = getString(R.string.terms15);
			lunarTerm[16] = getString(R.string.terms16);
			lunarTerm[17] = getString(R.string.terms17);
			lunarTerm[18] = getString(R.string.terms18);
			lunarTerm[19] = getString(R.string.terms19);
			lunarTerm[20] = getString(R.string.terms20);
			lunarTerm[21] = getString(R.string.terms21);
			lunarTerm[22] = getString(R.string.terms22);
			lunarTerm[23] = getString(R.string.terms23);
		}
	}
	
	private String getString(int id){
		return mContext.getString(id);
	}
	
	public String getTraditionalFestival(){
		return getTraditionalFestival(lunarYear, lunarMonth, lunarDay);
	}
	
	public String getTraditionalFestival(int lunarYear, int lunarMonth, int lunarDay) {
		if (lunarMonth == 1 && lunarDay == 1)
			return getString(R.string.chunjie);
		if (lunarMonth == 1 && lunarDay == 15)
			return getString(R.string.yuanxiao);
		if (lunarMonth == 5 && lunarDay == 5)
			return getString(R.string.duanwu);
		if (lunarMonth == 7 && lunarDay == 7)
			return getString(R.string.qixi);
		if (lunarMonth == 8 && lunarDay == 15)
			return getString(R.string.zhongqiu);
		if (lunarMonth == 9 && lunarDay == 9)
			return getString(R.string.chongyang);
		if (lunarMonth == 12 && lunarDay == 8)
			return getString(R.string.laba);
		if (lunarMonth == 12 && lunarDay == 23)
			return getString(R.string.xiaonian);

		if (lunarMonth == 12) {
			if (lunarDay == LunarCalendarConvertUtil.getLunarMonthDays(lunarYear, lunarMonth))
				return getString(R.string.chuxi);
		}
		return "";
	}
	
	public String getFestival(){
		return getFestival(solarMonth, solarDay);
	}

	public String getFestival(int lunarMonth, int lunarDay) {
		if (lunarMonth == 0 && lunarDay == 1)
			return getString(R.string.new_Year_day);
		if (lunarMonth == 1 && lunarDay == 14)
			return getString(R.string.valentin_day);
		if (lunarMonth == 2 && lunarDay == 8)
			return getString(R.string.women_day);
		if (lunarMonth == 2 && lunarDay == 12)
			return getString(R.string.arbor_day);
		if (lunarMonth == 4 && lunarDay == 1)
			return getString(R.string.labol_day);
		if (lunarMonth == 4 && lunarDay == 4)
			return getString(R.string.youth_day);
		if (lunarMonth == 5 && lunarDay == 1)
			return getString(R.string.children_day);
		if (lunarMonth == 7 && lunarDay == 1)
			return getString(R.string.army_day);
		if (lunarMonth == 8 && lunarDay == 10)
			return getString(R.string.teacher_day);
		if (lunarMonth == 9 && lunarDay == 1)
			return getString(R.string.national_day);
		if (lunarMonth == 11 && lunarDay == 25)
			return getString(R.string.christmas_day);
		return "";
	}
	
	
	public String getLunarSolarTerms(int year) {
	    int yy = 0;
	    double[] lunar_century_C;
        if(year > 1999){
            yy = year - 2000;
            lunar_century_C = LunarCalendarConvertUtil.lunar_21th_century_C;
            
        }else {
            yy = year - 1900;
            lunar_century_C = LunarCalendarConvertUtil.lunar_20th_century_C;
        }
        for (int i = 0; i < lunar_century_C.length; i++) {
            int day = 0;
            if(i <4){
                day = (int)Math.floor ( (yy*D_solar_terms+lunar_century_C[i])-((yy-1)/4));
            }else{
                day = (int)Math.floor ((yy*D_solar_terms+lunar_century_C[i])-(yy/4));
            }
            if(solarMonth == 0 && solarDay == day && i==0){
                return getString(R.string.terms0);
            }if(solarMonth == 0 && solarDay == day && i==1){
                return getString(R.string.terms1);
            }if(solarMonth == 1 && solarDay == day&& i==2){
                return getString(R.string.terms2);
            }if(solarMonth == 1 && solarDay == day&& i==3){
                return getString(R.string.terms3);
            }if(solarMonth == 2 && solarDay == day&& i==4){
                return getString(R.string.terms4);
            }if(solarMonth == 2 && solarDay == day&& i==5){
                return getString(R.string.terms5);
            }if(solarMonth == 3 && solarDay == day&& i==6){
                return getString(R.string.terms6);
            }if(solarMonth == 3 && solarDay == day&& i==7){
                return getString(R.string.terms7);
            }if(solarMonth == 4 && solarDay == day&& i==8){
                return getString(R.string.terms8);
            }if(solarMonth == 4 && solarDay == day&& i==9){
                return getString(R.string.terms9);
            }if(solarMonth == 5 && solarDay == day&& i==10){
                return getString(R.string.terms10);
            }if(solarMonth == 5 && solarDay == day&& i==11){
                return getString(R.string.terms11);
            }if(solarMonth == 6 && solarDay == day&& i==12){
                return getString(R.string.terms12);
            }if(solarMonth == 6 && solarDay == day&& i==13){
                return getString(R.string.terms13);
            }if(solarMonth == 7 && solarDay == day&& i==14){
                return getString(R.string.terms14);
            }if(solarMonth == 7 && solarDay == day&& i==15){
                return getString(R.string.terms15);
            }if(solarMonth == 8 && solarDay == day&& i==16){
                return getString(R.string.terms16);
            }if(solarMonth == 8 && solarDay == day&& i==17){
                return getString(R.string.terms17);
            }if(solarMonth == 9 && solarDay == day&& i==18){
                return getString(R.string.terms18);
            }if(solarMonth == 9 && solarDay == day&& i==19){
                return getString(R.string.terms19);
            }if(solarMonth == 10 && solarDay == day&& i==20){
                return getString(R.string.terms20);
            }if(solarMonth == 10 && solarDay == day&& i==21){
                return getString(R.string.terms21);
            }if(solarMonth == 11 && solarDay == day&& i==22){
                return getString(R.string.terms22);
            }if(solarMonth == 11 && solarDay == day&& i==23){
                return getString(R.string.terms23);
            }
        }
        return "";
    }
	
	final public String getAnimalsYear(){
		return getAnimalsYear(lunarYear);
	}
	
	final public String getAnimalsYear(int lunarYear) {
		return year_of_birth[(lunarYear - 4) % 12];
	}
	
	public String getChinaMonthString(){
		return getChinaMonthString(lunarMonth,isLeapMonth);
	}
	
	public String getChinaMonthString(int lunarMonth, boolean isLeapMonth) {
		String chinaMonth = (isLeapMonth ? lunarLeapTag : "")
				+ ((lunarMonth == 1) ? zhengyueTag : lunarCalendarNumber[lunarMonth - 1])
				+ lunarMonthTag;
		return chinaMonth;
	}
	
	public String getChinaDayString(boolean isDisplayLunarMonthForFirstDay){
		return getChinaDayString(lunarMonth, lunarDay, isLeapMonth, isDisplayLunarMonthForFirstDay);
	}
	
	public String getChinaDayString(int lunarMonth, int lunarDay, boolean isLeapMonth,
			boolean isDisplayLunarMonthForFirstDay) {
		if (lunarDay > 30)
			return "";
		if (lunarDay == 1 && isDisplayLunarMonthForFirstDay)
			return getChinaMonthString(lunarMonth, isLeapMonth);
		if (lunarDay == 10)
			return lunarCalendarTen[0] + lunarCalendarTen[1];
		if(lunarDay == 20)
			return lunarCalendarTen[4]+lunarCalendarTen[1];

		return lunarCalendarTen[lunarDay / 10] + lunarCalendarNumber[(lunarDay + 9) % 10];
	}
	
	public String getChinaYearString(){
		return getChinaYearString(lunarYear);
	}
	
	public String getChinaYearString(int lunarYear) {
		return String.valueOf(lunarYear);
	}

	public String[] getLunarCalendarInfo(){
		if(lunarYear ==0 || lunarMonth ==0 || lunarDay ==0)
			return null;//new String[]{null,null,null,null,null};
		String lunarYearStr = getChinaYearString();
		String lunarMonthStr = getChinaMonthString();
		String lunarDayStr = getChinaDayString(true);
		
		String traditionFestivalStr = getTraditionalFestival();
		String festivalStr = getFestival();
		String solarTermStr = getLunarSolarTerms(solarYear);
		
		return new String[]{lunarYearStr, lunarMonthStr, lunarDayStr, traditionFestivalStr, festivalStr,solarTermStr};
	}
	
    public boolean isLunarSetting() {
        String language = getLanguageEnv();

        if (language != null
                && (language.trim().equals("zh-CN") || language.trim().equals("zh-TW")))
            return true;
        else
            return false;
    }

    private String getLanguageEnv() {
        Locale l = Locale.getDefault();
        String language = l.getLanguage();
        String country = l.getCountry().toLowerCase();
        if ("zh".equals(language)) {
            if ("cn".equals(country)) {
                language = "zh-CN";
            } else if ("tw".equals(country)) {
                language = "zh-TW";
            }
        } else if ("pt".equals(language)) {
            if ("br".equals(country)) {
                language = "pt-BR";
            } else if ("pt".equals(country)) {
                language = "pt-PT";
            }
        }
        return language;
    }

	
}
