package com.rarnu.tools.neo.xposed;

public class XpStatus {

    public enum Mode { NDK, JVM }

    public static Mode mode = Mode.NDK;

    public static final String PKGNAME = "com.rarnu.tools.neo";
    public static final String PREF = "settings";

    public static final String KEY_REMOVESEARCHBAR = "removesearchbar";
    public static final String KEY_THEMECRACK = "themecrack";
    public static final String KEY_REMOVEAD = "removead";
    public static final String KEY_ROOTCRACK = "rootcrack";
    public static final String KEY_CORECRACK = "corecrack";
    public static final String KEY_NOUPDATE = "noupate";
    public static final String KEY_MINUS_SCREEN = "minusscreen";
    public static final String KEY_KEEP_MTZ = "keepmtz";

    // settings
    public static final String KEY_WORK_MODE = "work_mode";
    public static final String KEY_AD_CHOOSE = "ad_choose";
    public static final String KEY_DEEP_CLEAN = "deep_clean";

    // ad fucker for special apps
    public static final String KEY_AD_WEATHER = "fuck_weather";
    public static final String KEY_AD_MUSIC = "fuck_music";
    public static final String KEY_AD_VIDEO = "fuck_video";
    public static final String KEY_AD_SEARCHBOX = "fuck_searchbox";
    public static final String KEY_AD_MMS = "fuck_mms";
    public static final String KEY_AD_FILEEXPLORER = "fuck_fileexplorer";
    public static final String KEY_AD_DOWNLOAD = "fuck_download";
    public static final String KEY_AD_CLEANMASTER = "fuck_cleanmaster";
    public static final String KEY_AD_CALENDAR = "fuck_calendar";
    public static final String KEY_AD_BROWSER = "fuck_browser";
    public static final String KEY_AD_SYSTEM = "fuck_system";

    public static boolean canWriteSdcard = false;

    public static boolean isEnable() {
        return false;
    }

}
