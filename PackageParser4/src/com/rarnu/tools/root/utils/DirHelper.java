package com.rarnu.tools.root.utils;

import android.os.Environment;

import java.io.File;

public class DirHelper {

    private static final String SDCARD = "/sdcard";
    public static String SDCARD_DIR = "";
    public static String ROOT_DIR = "";
    public static String SYSAPP_DIR = "";
    public static String ENABLEAPP_DIR = "";
    public static String ENABLEAPP_DIR_SYSTEM = "";
    public static String ENABLEAPP_DIR_DATA = "";
    public static String DATAAPP_DIR = "";
    public static String BATCH_INSTALL_DIR = "";
    public static String HOSTS_DIR = "";
    public static String BUSYBOX_DIR = "";
    public static String MEM_DIR = "";
    public static String TEMP_DIR = "";
    public static String ERROR_DIR = "";
    public static String ICON_DIR = "";
    public static String FORCE_UPDATE_DIR = "";
    public static String DATA_SDCARD_DIR = "";
    public static String OBB_SDCARD_DIR = "";
    public static String GOOGLE_DIR = "";
    public static String FONT_DIR = "";
    public static String FONT_PREVIEW_DIR = "";

    public static void makeDir() {
        makeDir(ROOT_DIR);
        makeDir(SYSAPP_DIR);
        makeDir(ENABLEAPP_DIR);
        makeDir(ENABLEAPP_DIR_SYSTEM);
        makeDir(ENABLEAPP_DIR_DATA);
        makeDir(DATAAPP_DIR);
        makeDir(HOSTS_DIR);
        makeDir(MEM_DIR);
        makeDir(TEMP_DIR);
        makeDir(ERROR_DIR);
        makeDir(ICON_DIR);
        makeDir(FORCE_UPDATE_DIR);
        makeDir(BATCH_INSTALL_DIR);
        makeDir(GOOGLE_DIR);
        makeDir(FONT_DIR);
        makeDir(FONT_PREVIEW_DIR);
    }

    private static void makeDir(String path) {
        FileUtils.mkdir(path);
    }

    static {
        SDCARD_DIR = Environment.getExternalStorageDirectory().getPath();
        if (new File(SDCARD).exists()) {
            SDCARD_DIR = SDCARD;
        }
        ROOT_DIR = SDCARD_DIR + "/.root_tools/";

        SYSAPP_DIR = ROOT_DIR + "sysapp/";
        ENABLEAPP_DIR = ROOT_DIR + "enableapp/";
        ENABLEAPP_DIR_SYSTEM = ENABLEAPP_DIR + "system/";
        ENABLEAPP_DIR_DATA = ENABLEAPP_DIR + "data/";
        DATAAPP_DIR = ROOT_DIR + "data/";
        HOSTS_DIR = ROOT_DIR + "hosts/";
        BUSYBOX_DIR = ROOT_DIR + "busybox/";
        MEM_DIR = ROOT_DIR + "mem/";
        TEMP_DIR = ROOT_DIR + "tmp/";
        ERROR_DIR = ROOT_DIR + "error/";
        ICON_DIR = ROOT_DIR + "icon/";
        FORCE_UPDATE_DIR = ROOT_DIR + "force_update/";
        BATCH_INSTALL_DIR = SDCARD_DIR + "/install/";
        GOOGLE_DIR = ROOT_DIR + "google/";
        FONT_DIR = ROOT_DIR + "fonts/";
        FONT_PREVIEW_DIR = ROOT_DIR + "fonts_preview/";

        DATA_SDCARD_DIR = Environment.getExternalStorageDirectory().getPath() + "/Android/data/";
        OBB_SDCARD_DIR = Environment.getExternalStorageDirectory().getPath() + "/Android/obb/";
    }
}
