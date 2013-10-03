package com.yugioh.android.common;

import android.content.Context;
import com.rarnu.utils.ConfigUtils;

public class Config {

    private static final String KEY_FONT_SIZE = "key_font_size";
    private static final String KEY_AUTO_NAME = "key_auto_name";

    public static int cfgGetFontSize(Context context) {
        return ConfigUtils.getIntConfig(context, KEY_FONT_SIZE, 15);
    }

    public static void cfgSetFontSize(Context context, int value) {
        ConfigUtils.setIntConfig(context, KEY_FONT_SIZE, value);
    }

    public static boolean cfgGetAutoName(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_AUTO_NAME, true);
    }

    public static void cfgSetAutoName(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_AUTO_NAME, value);
    }
}
