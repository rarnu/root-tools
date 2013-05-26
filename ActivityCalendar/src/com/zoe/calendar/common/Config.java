package com.zoe.calendar.common;

import android.content.Context;

import com.rarnu.utils.ConfigUtils;

public class Config {

	private static final String KEY_CITY = "key_city";
	private static final String KEY_CITY_PINYIN = "key_city_pinyin";
	private static final String KEY_LAST_TIMESTAMP = "key_last_timestamp";

	public static String getCity(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_CITY, "");
	}

	public static void setCity(Context context, String city) {
		ConfigUtils.setStringConfig(context, KEY_CITY, city);
	}

	public static int getLastTimestamp(Context context) {
		return ConfigUtils.getIntConfig(context, KEY_LAST_TIMESTAMP, 0);
	}

	public static void setLastTimestamp(Context context, int timestamp) {
		ConfigUtils.setIntConfig(context, KEY_LAST_TIMESTAMP, timestamp);
	}

	public static String getCityPinyin(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_CITY_PINYIN, "");
	}

	public static void setCityPinyin(Context context, String pinyin) {
		ConfigUtils.setStringConfig(context, KEY_CITY_PINYIN, pinyin);
	}
}
