package com.rarnu.utils;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
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

    public static float getFloatConfig(Context context, String key, float def) {
        initSharedPreference(context);
        return sp.getFloat(key, def);
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

    public static void setFloatConfig(Context context, String key, float value) {
        initSharedPreference(context);
        sp.edit().putFloat(key, value).commit();
    }

    private static ApplicationInfo getApplicationInfo(Context context) {
        ApplicationInfo info = null;
        try {
            info = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA);
        } catch (Exception e) {

        }
        return info;
    }

    public static int getManifestIntConfig(Context context, String key, int def) {
        int ret = def;
        ApplicationInfo appInfo = getApplicationInfo(context);
        if (appInfo != null) {
            ret = appInfo.metaData.getInt(key, def);
        }
        return ret;
    }

    public static boolean getManifestBooleanConfig(Context context, String key, boolean def) {
        boolean ret = def;
        ApplicationInfo appInfo = getApplicationInfo(context);
        if (appInfo != null) {
            ret = appInfo.metaData.getBoolean(key, def);
        }
        return ret;
    }

    public static String getManifestStringConfig(Context context, String key, String def) {
        String ret = def;
        ApplicationInfo appInfo = getApplicationInfo(context);
        if (appInfo != null) {
            ret = appInfo.metaData.getString(key, def);
        }
        return ret;
    }


    public static long getManifestLongConfig(Context context, String key, long def) {
        long ret = def;
        ApplicationInfo appInfo = getApplicationInfo(context);
        if (appInfo != null) {
            ret = appInfo.metaData.getLong(key, def);
        }
        return ret;
    }

    public static float getManifestFloatConfig(Context context, String key, float def) {
        float ret = def;
        ApplicationInfo appInfo = getApplicationInfo(context);
        if (appInfo != null) {
            ret = appInfo.metaData.getFloat(key, def);
        }
        return ret;
    }
}
