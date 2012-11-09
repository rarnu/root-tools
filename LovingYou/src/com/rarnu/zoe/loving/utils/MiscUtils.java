package com.rarnu.zoe.loving.utils;

import java.util.Calendar;

public class MiscUtils {

	public static Calendar loadTimeMillis(long time) {
		Calendar c = Calendar.getInstance();
		c.setTimeInMillis(time);
		return c;
	}

}
