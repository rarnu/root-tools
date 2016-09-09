package com.rarnu.tools.neo.api;

public class NativeAPI {
    static {
        System.loadLibrary("cmd");
    }

    public static native boolean freezeApplication(String packageName, boolean isFreezed);
}

