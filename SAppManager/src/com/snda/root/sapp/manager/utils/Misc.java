package com.snda.root.sapp.manager.utils;

import android.content.Context;
import android.content.SharedPreferences;

public class Misc {
	public static String getConfig(Context context, String key, String def) {
		SharedPreferences pre = context.getSharedPreferences("SAPPMANAGER", 0);
		return pre.getString(key, def);
	}
	
	public static void setConfig(Context context, String key, String val) {
		context.getSharedPreferences("SAPPMANAGER", 0).edit().putString(key, val).commit();
	}
}
