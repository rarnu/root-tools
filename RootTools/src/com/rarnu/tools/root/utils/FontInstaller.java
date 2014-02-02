package com.rarnu.tools.root.utils;

import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.FallbackFontItem;
import com.rarnu.tools.root.common.FontItem;
import com.rarnu.tools.root.common.SystemFontItem;
import com.rarnu.utils.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class FontInstaller {

    private static final String SYSTEM_FONT_PATH = "/system/fonts/";
    private static String[] REPLACE_FONTS = new String[]{
            "Roboto-Regular.ttf",
            "RobotoCondensed-Regular.ttf",
            "DroidSerif-Regular.ttf",
            "DroidSans.ttf",
            "DroidSansMono.ttf",
            "DroidSansFallback.ttf"
    };

    public static void installFont(final FontItem item) {

        String tmpFontName = DirHelper.FONT_DIR + "temp.ttf";
        File fTmpFont = new File(tmpFontName);
        if (fTmpFont.exists()) {
            fTmpFont.delete();
        }
        try {
            FileUtils.copyFile(DirHelper.FONT_DIR + item.fileName, tmpFontName, null);
        } catch (Exception e) {

        }

        for (String rf : REPLACE_FONTS) {
            if (new File(SYSTEM_FONT_PATH + rf).exists()) {
                RootUtils.runCommand(String.format("busybox cp -f %s %s", tmpFontName, SYSTEM_FONT_PATH + rf), true);
                RootUtils.runCommand(String.format("busybox chmod 666 %s", SYSTEM_FONT_PATH + rf), true);
            }
        }
        RootUtils.runCommand(String.format("busybox cp -f %s /system/etc/", DirHelper.FONT_DIR + FontUtils.FALLBACK_FONTS_XML), true);
        RootUtils.runCommand(String.format("busybox cp -f %s /system/etc/", DirHelper.FONT_DIR + FontUtils.SYSTEM_FONTS_XML), true);
        RootUtils.runCommand(String.format("busybox chmod 666 /system/etc/%s", FontUtils.FALLBACK_FONTS_XML), true);
        RootUtils.runCommand(String.format("busybox chmod 666 /system/etc/%s", FontUtils.SYSTEM_FONTS_XML), true);

    }

    public static boolean isFontInstalled(final FontItem item) {
        // check install status
        File fFont = new File(DirHelper.FONT_DIR + item.fileName);
        long fileSize = fFont.length();
        boolean ret = true;
        for (String fn : REPLACE_FONTS) {
            if (fileSize != new File("/system/fonts/" + fn).length()) {
                ret = false;
                break;
            }
        }
        return ret;
    }

    public static boolean isBackuped() {
        boolean ret = false;
        String fFontBackup = DirHelper.FONT_DIR + "backup/";
        File fBackup = new File(fFontBackup);
        if (fBackup.exists()) {
            String[] files = fBackup.list();
            if (files != null && files.length != 0) {
                ret = true;
            }
        }
        return ret;
    }

    public static void backupFonts(List<FallbackFontItem> listFallback, List<SystemFontItem> listSystem) {
        // backup fonts
        String fFontBackup = DirHelper.FONT_DIR + "backup/";
        File fBackup = new File(fFontBackup);
        if (!fBackup.exists()) {
            fBackup.mkdirs();
        }

        try {
            List<String> listFonts = new ArrayList<String>();
            for (FallbackFontItem item : listFallback) {
                if (item.exist) {
                    listFonts.add(item.fileName);
                }
            }
            for (SystemFontItem item : listSystem) {
                if (item.exist) {
                    listFonts.add(item.file);
                }
            }
            for (String fn : listFonts) {
                RootUtils.runCommand(String.format("busybox cp -f /system/fonts/%s %s", fn, fFontBackup), true);
            }
        } catch (Exception e) {

        }
    }

    public static void restoreFont() {
        // restore fonts
        String fFontBackup = DirHelper.FONT_DIR + "backup/";
        File fBackup = new File(fFontBackup);
        if (fBackup.exists()) {
            String[] files = fBackup.list();
            if (files != null && files.length != 0) {
                for (String fn : files) {
                    RootUtils.runCommand(String.format("busybox cp -f %s%s /system/fonts/", fFontBackup, fn), true);
                    RootUtils.runCommand(String.format("chmod 666 /system/fonts/%s", fn), true);
                }
            }
        }

    }

}
