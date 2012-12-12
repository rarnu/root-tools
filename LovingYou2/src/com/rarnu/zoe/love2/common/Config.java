package com.rarnu.zoe.love2.common;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

public class Config {

	// need update when app start
	public static String TOKEN = "";
	public static long EXPRIED = 0;

	public static final String FIRST_START = "first_start";
	private static final String HINT_ENABLED = "hint%d_enabled";
	private static final String HINT_TIME = "hint%d_time";
	private static final String SINA_TOKEN = "sina_token";
	private static final String SINA_EXPIRED = "sina_expired";
	private static final String LAST_TEXT = "last_text";

	private static SharedPreferences sp = null;

	private static void initSharedPreference(Context context) {
		if (sp == null) {
			sp = PreferenceManager.getDefaultSharedPreferences(context);
		}
	}

	public static boolean getFirstStart(Context context) {
		initSharedPreference(context);
		return sp.getBoolean(FIRST_START, true);
	}

	public static void setFirstStart(Context context, boolean value) {
		initSharedPreference(context);
		sp.edit().putBoolean(FIRST_START, value).commit();
	}

	public static boolean getHintEnabled(Context context, int index) {
		initSharedPreference(context);
		String key = String.format(HINT_ENABLED, index);
		return sp.getBoolean(key, false);
	}

	public static void setHintEnabled(Context context, int index,
			boolean enabled) {
		initSharedPreference(context);
		String key = String.format(HINT_ENABLED, index);
		sp.edit().putBoolean(key, enabled).commit();
	}

	public static long getHintTime(Context context, int index, long def) {
		initSharedPreference(context);
		String key = String.format(HINT_TIME, index);
		return sp.getLong(key, def);
	}

	public static void setHintTime(Context context, int index, long value) {
		initSharedPreference(context);
		String key = String.format(HINT_TIME, index);
		sp.edit().putLong(key, value).commit();
	}
	
	public static String getSinaToken(Context context) {
		initSharedPreference(context);
		return sp.getString(SINA_TOKEN, "");
	}
	
	public static void setSinaToken(Context context, String value) {
		initSharedPreference(context);
		sp.edit().putString(SINA_TOKEN, value).commit();
	}
	
	public static long getSinaExpired(Context context) {
		initSharedPreference(context);
		return sp.getLong(SINA_EXPIRED, 0);
	}
	
	public static void setSinaExpired(Context context, long value) {
		initSharedPreference(context);
		sp.edit().putLong(SINA_EXPIRED, value).commit();
	}
	
	public static String getLastText(Context context) {
		initSharedPreference(context);
		return sp.getString(LAST_TEXT, "");
	}
	
	public static void setLastText(Context context, String value) {
		initSharedPreference(context);
		sp.edit().putString(LAST_TEXT, value).commit();
	}
}


