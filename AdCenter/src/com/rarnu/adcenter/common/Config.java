package com.rarnu.adcenter.common;

import com.rarnu.utils.ConfigUtils;

import android.content.Context;

public class Config {

	private static final String KEY_CURRENT_AREA = "key_current_area";
	
	public static int getCurrentArea(Context context) {
		return ConfigUtils.getIntConfig(context, KEY_CURRENT_AREA, 0);
	}
	
	public static void setCurrentArea(Context context, int value) {
		ConfigUtils.setIntConfig(context, KEY_CURRENT_AREA, value);
	}
}
