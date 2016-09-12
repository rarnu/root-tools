package com.rarnu.tools.neo.api;

import android.content.Context;

public class NativeAPI {
    static {
        System.loadLibrary("cmd");
    }

    public static native boolean freezeApplication(String packageName, boolean isFreezed);
    public static native boolean freezeComponent(String packageName, String componentName, boolean isFreezed);
    public static native boolean freezeComponents(String packageName, String[] componentNames, boolean isFreezed);
    public static native void systemClean(Context ctx);


    public static final int STATUS_PROGRESS = 0;
    public static final int STATUS_COMPLETE = 1;
    public static final int STATUS_ERROR = 2;
    
    public void cleanCallback(Context ctx, int status, String data) {
        // TODO: callback
    }
}

