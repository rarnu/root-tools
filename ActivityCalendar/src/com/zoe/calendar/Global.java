package com.zoe.calendar;

import com.baidu.location.BDLocation;
import com.zoe.calendar.location.LocationProvider;

public class Global {

	public static String city = "";
	public static String city_pinyin = "";

	public static boolean[] settingTypes = new boolean[8];

	public static LocationProvider locProvider = null;
	public static BDLocation location = null;
	public static boolean synced = false;

	public static long newTimestamp = 0;
	public static long newAllTimestamp = 0;

	public static String iconFilePath = "";

}
