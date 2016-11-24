package com.rarnu.tools.neo.api;

import android.content.Context;
import android.content.Intent;
import com.rarnu.tools.neo.fragment.CleanFragment;

public class NativeAPI {
    static {
        System.loadLibrary("cmd");
    }

    public static boolean isRejected = false;
    public static boolean isSystemRW = false;

    public static native boolean mount();
    public static native boolean isSystemRW();
    public static native void makePreferenceReadable(int sdk, String packageName);
    public static native boolean freezeApplication(String packageName, boolean isFreezed);
    public static native boolean freezeComponent(String packageName, String componentName, boolean isFreezed);
    public static native boolean freezeComponents(String packageName, String[] componentNames, boolean isFreezed);
    public static native void systemClean(Context ctx);
    public static native boolean writeFile(Context ctx, String filePath, String text, int perm);
    public static native boolean catFile(String src, String dest, int perm);
    public static native void forceDeleteFile(String path);
    public static native void forceDropCache();
    public static native void killProcess();
    public static native boolean deleteSystemApp(String pkgName);
    public static native boolean isAppRequiredBySystem(String pkgName);
    
    public void cleanCallback(Context ctx, int status, String data) {
        DeviceAPI.cleanCallback(ctx, status, data);
    }
}

