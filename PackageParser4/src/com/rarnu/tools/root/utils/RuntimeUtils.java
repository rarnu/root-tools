package com.rarnu.tools.root.utils;

import android.os.Build;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;

import java.io.File;

public class RuntimeUtils {

    private static final String LIB_ART = "/system/lib/libart.so";
    private static final String LIB_DALVIK = "/system/lib/libdvm.so";
    private static final String PROP_NAME = "persist.sys.dalvik.vm.lib";
    private static final String RT_DALVIK = "libdvm.so";
    private static final String RT_ART = "libart.so";

    public static boolean isAndroidL() {
        return Build.VERSION.SDK_INT >= 20;
    }

    public static boolean hasArt() {
        boolean ret = false;
        if (Build.VERSION.SDK_INT >= 19) {
            File fArt = new File(LIB_ART);
            ret = fArt.exists();
        }
        return ret;
    }

    public static boolean hasDalvik() {
        File fDalvik = new File(LIB_DALVIK);
        boolean ret = fDalvik.exists();
        return ret;
    }

    public static boolean isArtMode() {
        CommandResult ret = RootUtils.runCommand(String.format("getprop %s", PROP_NAME), false);
        return (ret != null && ret.result.toLowerCase().contains("art"));
    }

    public static boolean switchRuntime(boolean useArt) {
        CommandResult ret = RootUtils.runCommand(String.format("setprop %s %s", PROP_NAME, (useArt ? RT_ART : RT_DALVIK)), true);
        boolean isArt = isArtMode();
        return (useArt && isArt) || (!useArt && !isArt);
    }
}
