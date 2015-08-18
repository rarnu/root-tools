package com.rarnu.utils;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.preference.PreferenceManager;

import java.lang.reflect.Field;

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

    public static void getObjectConfig(Context context, String key, Object obj) {
        initSharedPreference(context);
        Field[] fs = ReflectionUtils.getClassFields(obj);
        String typeStr = "";
        String keyStr = "";
        for (Field f: fs) {
            typeStr = f.getType().getSimpleName();
            keyStr = String.format("%s_%s", key, f.getName());
            if (typeStr.equals("String")) {
                try { f.set(obj, getStringConfig(context, keyStr, "")); } catch(Exception e) { }
            } else if (typeStr.equals("int")) {
                try { f.setInt(obj, getIntConfig(context, keyStr, 0)); } catch(Exception e) {}
            } else if (typeStr.equals("double")) {
                try { f.setDouble(obj, getFloatConfig(context, keyStr, 0)); } catch(Exception e) {}
            } else if (typeStr.equals("boolean")) {
                try { f.setBoolean(obj, getBooleanConfig(context, keyStr, false)); } catch(Exception e) {}
            } else if (typeStr.equals("float")) {
                try { f.setFloat(obj, getFloatConfig(context, keyStr, 0)); } catch(Exception e) {}
            } else if (typeStr.equals("long")) {
                try { f.setLong(obj, getLongConfig(context, keyStr, 0)); } catch(Exception e) {}
            } else if (typeStr.equals("byte")) {
                try { f.setByte(obj, (byte)getIntConfig(context, keyStr, 0)); } catch(Exception e) {}
            } else if (typeStr.equals("short")) {
                try { f.setShort(obj, (short)getIntConfig(context, keyStr, 0)); } catch(Exception e) {}
            } else if (typeStr.equals("char")) {
                try { f.setChar(obj, getStringConfig(context, keyStr, "").charAt(0)); } catch(Exception e) {}
            }
        }
    }

    public static void setObjectConfig(Context context, String key, Object obj) {
        initSharedPreference(context);
        Field[] fs = ReflectionUtils.getClassFields(obj);
        String typeStr = "";
        String keyStr = "";
        for (Field f: fs) {
            typeStr = f.getType().getSimpleName();
            keyStr = String.format("%s_%s", key, f.getName());
            if (typeStr.equals("String")) {
                try { setStringConfig(context, keyStr, (String)f.get(obj)); } catch (Exception e) {}
            } else if (typeStr.equals("int")) {
                try { setIntConfig(context, keyStr, f.getInt(obj)); } catch (Exception e) {}
            } else if (typeStr.equals("double")) {
                try { setFloatConfig(context, keyStr, f.getFloat(obj)); } catch (Exception e) {}
            } else if (typeStr.equals("boolean")) {
                try { setBooleanConfig(context, keyStr, f.getBoolean(obj)); } catch (Exception e) {}
            } else if (typeStr.equals("float")) {
                try { setFloatConfig(context, keyStr, (float) f.getDouble(obj)); } catch (Exception e) {}
            } else if (typeStr.equals("long")) {
                try { setLongConfig(context, keyStr, f.getLong(obj)); } catch (Exception e) {}
            } else if (typeStr.equals("byte")) {
                try { setIntConfig(context, keyStr, f.getByte(obj)); } catch (Exception e) {}
            } else if (typeStr.equals("short")) {
                try { setIntConfig(context, keyStr, f.getShort(obj)); } catch (Exception e) {}
            } else if (typeStr.equals("char")) {
                try { setStringConfig(context, keyStr, String.valueOf(f.getChar(obj))); } catch (Exception e) {}
            }
        }
    }
}
