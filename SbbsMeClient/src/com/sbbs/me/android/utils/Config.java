package com.sbbs.me.android.utils;

import android.content.Context;

import com.rarnu.utils.ConfigUtils;

public class Config {

	private static final String KEY_USER_ID = "user_id";

	public static String getUserId(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_USER_ID, "");
	}

	public static void setUserId(Context context, String value) {
		ConfigUtils.setStringConfig(context, KEY_USER_ID, value);
	}
}
