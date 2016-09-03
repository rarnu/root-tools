package com.rarnu.tools.neo.xposed;

public class XpStatus {

    public static final String PKGNAME = "com.rarnu.tools.neo";
    public static final String PREF = "settings";

    public static final String KEY_REMOVESEARCHBAR = "removesearchbar";
    public static final String KEY_THEMECRACK = "themecrack";
    public static final String KEY_REMOVEAD = "removead";
    public static final String KEY_ROOTCRACK = "rootcrack";
    public static final String KEY_CORECRACK = "corecrack";
    public static final String KEY_NOUPDATE = "noupate";
    public static final String KEY_FAKE_DEVICE = "fakedevice";
    public static final String KEY_FAKE_DEVICE_ID = "fakedevice_id";
    public static final String KEY_FAKE_DEVICE_NAME = "fakedevice_name";

    public static boolean canWriteSdcard = false;

    public static boolean isEnable() {
        return false;
    }

}
