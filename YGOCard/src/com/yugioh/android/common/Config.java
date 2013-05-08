package com.yugioh.android.common;

import android.content.Context;

import com.rarnu.devlib.utils.ConfigUtils;

public class Config {

	private static final String KEY_FONT_SIZE = "key_font_size";
	
	public static int cfgGetFontSize(Context context) {
		return ConfigUtils.getIntConfig(context, KEY_FONT_SIZE, 15);
	}

	public static void cfgSetFontSize(Context context, int value) {
		ConfigUtils.setIntConfig(context, KEY_FONT_SIZE, value);
	}
}
