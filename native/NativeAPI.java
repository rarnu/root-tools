package com.rarnu.tools.neo.api;

public class NativeAPI {
    static {
        System.loadLibrary("cmd");
    }

    public static native boolean freezeApplication(String packageName, boolean isFreezed);
    public static native boolean freezeComponent(String packageName, String componentName, boolean isFreezed);
    public static native boolean freezeComponents(String packageName, String[] componentNames, boolean isFreezed);
}

