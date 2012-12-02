package com.rarnu.zoe.love2.utils;

import java.util.Calendar;

public class MiscUtils {

	public static Calendar loadTimeMillis(long time) {
		Calendar c = Calendar.getInstance();
		c.setTimeInMillis(time);
		return c;
	}
	
	public static Calendar loadDefaultCalendar(int hour) {
		Calendar cDef = Calendar.getInstance();
		cDef.set(Calendar.HOUR_OF_DAY, hour);
		cDef.set(Calendar.MINUTE, 0);
		return cDef;
	}

}
