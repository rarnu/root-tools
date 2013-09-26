package com.rarnu.utils;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

public class ConfigUtils {

    private static SharedPreferences sp = null;

    private static void initSharedPreference(Context context) {
        if (sp == null) {
            sp = PreferenceManager.getDefaultSharedPreferences(context);
        }
    }

    public static String getStringConfig(Context context, String key, String def) {
        initSharedPreference(context);
        return sp.getString(key, def);
    }

    public static int getIntConfig(Context context, String key, int def) {
        initSharedPreference(context);
        return sp.getInt(key, def);
    }

    public static long getLongConfig(Context context, String key, long def) {
        initSharedPreference(context);
        return sp.getLong(key, def);
    }

    public static boolean getBooleanConfig(Context context, String key, boolean def) {
        initSharedPreference(context);
        return sp.getBoolean(key, def);
    }

    public static void setStringConfig(Context context, String key, String value) {
        initSharedPreference(context);
        sp.edit().putString(key, value).commit();
    }

    public static void setIntConfig(Context context, String key, int value) {
        initSharedPreference(context);
        sp.edit().putInt(key, value).commit();
    }

    public static void setLongConfig(Context context, String key, long value) {
        initSharedPreference(context);
        sp.edit().putLong(key, value).commit();
    }

    public static void setBooleanConfig(Context context, String key, boolean value) {
        initSharedPreference(context);
        sp.edit().putBoolean(key, value).commit();
    }

}
