package com.zoe.calendar.common;

import android.content.Context;

import com.rarnu.utils.ConfigUtils;
import com.zoe.calendar.R;

public class Config {

	private static final String KEY_CITY = "key_city";
	private static final String KEY_CITY_PINYIN = "key_city_pinyin";
	private static final String KEY_LAST_TIMESTAMP = "key_last_timestamp";
	private static final String KEY_LAST_CALENDAR_TIMESTAMP = "key_last_calendar_timestamp";
	private static final String KEY_SETTING_TYPE = "key_setting_type_%d";
	private static final String KEY_MOTION = "key_motion_%d";
	private static final String KEY_LAST_WEATHER_TIMESTAMP = "key_last_weather_timestamp";
	private static final String KEY_LAST_WEATHER_STRING = "key_last_weather_string";
	private static final String KEY_LAST_WEATHER_TEMP = "key_last_weather_temp";
	private static final String KEY_IS_FIRST_START = "key_is_first_start";

	public static String getCity(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_CITY, "");
	}

	public static void setCity(Context context, String city) {
		ConfigUtils.setStringConfig(context, KEY_CITY, city);
	}

	public static long getLastTimestamp(Context context, String city) {
		return ConfigUtils.getLongConfig(context, KEY_LAST_TIMESTAMP + "_"
				+ city, 0L);
	}

	public static void setLastTimestamp(Context context, String city,
			long timestamp) {
		ConfigUtils.setLongConfig(context, KEY_LAST_TIMESTAMP + "_" + city,
				timestamp);
	}
	
	public static long getLastCalendarTimestamp(Context context) {
		return ConfigUtils.getLongConfig(context, KEY_LAST_CALENDAR_TIMESTAMP, 0L);
	}

	public static void setLastCalendarTimestamp(Context context, long timestamp) {
		ConfigUtils.setLongConfig(context, KEY_LAST_CALENDAR_TIMESTAMP, timestamp);
	}
	
	public static String getCityPinyin(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_CITY_PINYIN, "");
	}

	public static void setCityPinyin(Context context, String pinyin) {
		ConfigUtils.setStringConfig(context, KEY_CITY_PINYIN, pinyin);
	}

	public static boolean getSettingType(Context context, int index) {
		return ConfigUtils.getBooleanConfig(context,
				String.format(KEY_SETTING_TYPE, index), true);
	}

	public static void setSettingType(Context context, int index, boolean value) {
		ConfigUtils.setBooleanConfig(context,
				String.format(KEY_SETTING_TYPE, index), value);
	}

	public static int getMotion(Context context, int index) {
		return ConfigUtils.getIntConfig(context,
				String.format(KEY_MOTION, index), 0);
	}

	public static void setMotion(Context context, int index, int value) {
		ConfigUtils.setIntConfig(context, String.format(KEY_MOTION, index),
				value);
	}

	public static long getLastWeatherTimestamp(Context context, String city) {
		return ConfigUtils.getLongConfig(context, KEY_LAST_WEATHER_TIMESTAMP
				+ "_" + city, 0L);
	}

	public static void setLastWeatherTimestamp(Context context, String city,
			long value) {
		ConfigUtils.setLongConfig(context, KEY_LAST_WEATHER_TIMESTAMP + "_"
				+ city, value);
	}

	public static String getLastWeatherString(Context context, String city) {
		return ConfigUtils.getStringConfig(context, KEY_LAST_WEATHER_STRING
				+ "_" + city, "");
	}

	public static void setLastWeatherString(Context context, String city,
			String value) {
		ConfigUtils.setStringConfig(context, KEY_LAST_WEATHER_STRING + "_"
				+ city, value);
	}

	public static String getLastWeatherTemp(Context context, String city) {
		return ConfigUtils.getStringConfig(context, KEY_LAST_WEATHER_TEMP + "_"
				+ city, "");
	}

	public static void setLastWeatherTemp(Context context, String city,
			String value) {
		ConfigUtils.setStringConfig(context,
				KEY_LAST_WEATHER_TEMP + "_" + city, value);
	}

	public static boolean getIsFirstStart(Context context) {
		return ConfigUtils.getBooleanConfig(context, KEY_IS_FIRST_START, true);
	}

	public static void setIsFirstStart(Context context, boolean value) {
		ConfigUtils.setBooleanConfig(context, KEY_IS_FIRST_START, value);
	}

	public static String loadFiltedString(Context context) {
		String[] str = context.getResources().getStringArray(
				R.array.settings_types);
		String ret = "";
		for (int i = 0; i < str.length; i++) {
			if (getSettingType(context, i)) {
				ret += str[i];
			}
		}
		return ret;
	}
}
