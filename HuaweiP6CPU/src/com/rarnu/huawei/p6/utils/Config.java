package com.rarnu.huawei.p6.utils;

import android.content.Context;
import com.rarnu.utils.ConfigUtils;

public class Config {
    private static final String KEY_CPU_FREQ = "key_cpu_freq";
    private static final String KEY_CPU_CORE = "key_cpu_core";
    private static final String KEY_DDR_FREQ = "key_ddr_freq";
    private static final String KEY_GPU_FREQ = "key_gpu_freq";
    private static final String KEY_PROFILE = "key_profile";

    public static String getCpuFreq(Context context) {
        return ConfigUtils.getStringConfig(context, KEY_CPU_FREQ, "0");
    }

    public static void setCpuFreq(Context context, String value) {
        ConfigUtils.setStringConfig(context, KEY_CPU_FREQ, value);
    }

    public static String getCpuCore(Context context) {
        return ConfigUtils.getStringConfig(context, KEY_CPU_CORE, "0");
    }

    public static void setCpuCore(Context context, String value) {
        ConfigUtils.setStringConfig(context, KEY_CPU_CORE, value);
    }

    public static String getDdrFreq(Context context) {
        return ConfigUtils.getStringConfig(context, KEY_DDR_FREQ, "0");
    }

    public static void setDdrFreq(Context context, String value) {
        ConfigUtils.setStringConfig(context, KEY_DDR_FREQ, value);
    }

    public static String getGpuFreq(Context context) {
        return ConfigUtils.getStringConfig(context, KEY_GPU_FREQ, "0");
    }

    public static void setGpuFreq(Context context, String value) {
        ConfigUtils.setStringConfig(context, KEY_GPU_FREQ, value);
    }

    public static int getProfile(Context context) {
        return ConfigUtils.getIntConfig(context, KEY_PROFILE, 0);
    }

    public static void setProfile(Context context, int value) {
        ConfigUtils.setIntConfig(context, KEY_PROFILE, value);
    }

}
