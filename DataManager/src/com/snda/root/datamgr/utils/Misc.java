package com.snda.root.datamgr.utils;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

public class Misc {
	public static String getConfig(Context context, String key, String def) {
		SharedPreferences pre = PreferenceManager
				.getDefaultSharedPreferences(context);
		return pre.getString(key, def);
	}

	public static int getConfig(Context context, String key, int def) {
		SharedPreferences pre = PreferenceManager
				.getDefaultSharedPreferences(context);
		return pre.getInt(key, def);
	}

	public static boolean getConfig(Context context, String key, boolean def) {
		SharedPreferences pre = PreferenceManager
				.getDefaultSharedPreferences(context);
		return pre.getBoolean(key, def);
	}

	public static void setConfig(Context context, String key, String val) {
		PreferenceManager.getDefaultSharedPreferences(context).edit()
				.putString(key, val).commit();
	}

	public static void setConfig(Context context, String key, int val) {
		PreferenceManager.getDefaultSharedPreferences(context).edit().putInt(
				key, val).commit();
	}

	public static void setConfig(Context context, String key, boolean val) {
		PreferenceManager.getDefaultSharedPreferences(context).edit()
				.putBoolean(key, val).commit();
	}
}
