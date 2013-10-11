package com.rarnu.ucloud.android.common;

import android.content.Context;
import com.rarnu.utils.ConfigUtils;

public class Config {

    private static final String KEY_NOTIFY_SERVER_DOWN = "key_notify_server_down";
    private static final String KEY_NOTIFY_DISK_USAGE = "key_notify_disk_usage";
    private static final String KEY_NOTIFY_FLOW_USAGE = "key_notify_flow_usage";
    private static final String KEY_NOTIFY_COST_USAGE = "key_notify_cost_usage";
    private static final String KEY_NOTIFY_SERVICE_RECEIVED = "key_notify_service_received";
    private static final String KEY_SERVICE_RINGTONE = "key_service_ringtone";
    private static final String KEY_SERVICE_VIBRATE = "key_service_vibrate";
    private static final String KEY_SERVICE_LED = "key_service_led";
    private static final String KEY_DISK_PERCENT = "key_disk_percent";
    private static final String KEY_FLOW_PERCENT = "key_flow_percent";
    private static final String KEY_COST_PERCENT = "key_cost_percent";
    private static final String KEY_RINGTONE_URI = "key_ringtone_uri";

    public static boolean getNotifyServerDown(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_NOTIFY_SERVER_DOWN, true);
    }

    public static void setNotifyServerDown(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_NOTIFY_SERVER_DOWN, value);
    }

    public static boolean getNotifyDiskUsage(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_NOTIFY_DISK_USAGE, true);
    }

    public static void setNotifyDiskUsage(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_NOTIFY_DISK_USAGE, value);
    }

    public static boolean getNotifyFlowUsage(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_NOTIFY_FLOW_USAGE, true);
    }

    public static void setNotifyFlowUsage(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_NOTIFY_FLOW_USAGE, value);
    }

    public static boolean getNotifyCostUsage(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_NOTIFY_COST_USAGE, true);
    }

    public static void setNotifyCostUsage(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_NOTIFY_COST_USAGE, value);
    }

    public static boolean getNotifyServiceReceived(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_NOTIFY_SERVICE_RECEIVED, true);
    }

    public static void setNotifyServiceReceived(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_NOTIFY_SERVICE_RECEIVED, value);
    }

    public static boolean getServiceRingtone(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_SERVICE_RINGTONE, true);
    }

    public static void setServiceRingtone(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_SERVICE_RINGTONE, value);
    }

    public static boolean getServiceVibrate(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_SERVICE_VIBRATE, true);
    }

    public static void setServiceVibrate(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_SERVICE_VIBRATE, value);
    }

    public static boolean getServiceLed(Context context) {
        return ConfigUtils.getBooleanConfig(context, KEY_SERVICE_LED, true);
    }

    public static void setServiceLed(Context context, boolean value) {
        ConfigUtils.setBooleanConfig(context, KEY_SERVICE_LED, value);
    }

    public static int getDiskPercent(Context context) {
        return ConfigUtils.getIntConfig(context, KEY_DISK_PERCENT, 90);
    }

    public static void setDiskPercent(Context context, int value) {
        ConfigUtils.setIntConfig(context, KEY_DISK_PERCENT, value);
    }

    public static int getFLowPercent(Context context) {
        return ConfigUtils.getIntConfig(context, KEY_FLOW_PERCENT, 90);
    }

    public static void setFlowPercent(Context context, int value) {
        ConfigUtils.setIntConfig(context, KEY_FLOW_PERCENT, value);
    }

    public static int getCostPercent(Context context) {
        return ConfigUtils.getIntConfig(context, KEY_COST_PERCENT, 95);
    }

    public static void setCostPercent(Context context, int value) {
        ConfigUtils.setIntConfig(context, KEY_COST_PERCENT, value);
    }

    public static String getRingtoneUri(Context context) {
        return ConfigUtils.getStringConfig(context, KEY_RINGTONE_URI, "");
    }

    public static void setRingtoneUri(Context context, String value) {
        ConfigUtils.setStringConfig(context, KEY_RINGTONE_URI, value);
    }
}
