package com.rarnu.fontter;

import android.content.Context;
import com.rarnu.command.RootUtils;
import com.rarnu.fontter.api.FontAPI;
import com.rarnu.fontter.api.FontItem;
import com.rarnu.utils.FileUtils;

import java.io.File;

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

    public static void installFont(final Context context, final FontItem item) {
        String tmpFontName = FontAPI.TMP_DIR + "temp.ttf";
        File fTmpFont = new File(tmpFontName);
        if (fTmpFont.exists()) {
            fTmpFont.delete();
        }
        try {
            FileUtils.copyFile(FontAPI.TMP_DIR + item.fileName, tmpFontName, null);
        } catch (Exception e) {

        }

        for (int i = 0; i < REPLACE_FONTS.length; i++) {
            if (new File(SYSTEM_FONT_PATH + REPLACE_FONTS[i]).exists()) {
                RootUtils.runCommand(String.format("busybox cp -f %s %s", tmpFontName, SYSTEM_FONT_PATH + REPLACE_FONTS[i]), true);
                RootUtils.runCommand(String.format("busybox chmod 666 %s", SYSTEM_FONT_PATH + REPLACE_FONTS[i]), true);
            }
        }
        RootUtils.runCommand(String.format("busybox cp -f %s /system/etc/", FontAPI.TMP_DIR + FontUtils.FALLBACK_FONTS_XML), true);
        RootUtils.runCommand(String.format("busybox cp -f %s /system/etc/", FontAPI.TMP_DIR + FontUtils.SYSTEM_FONTS_XML), true);
        RootUtils.runCommand(String.format("busybox chmod 666 /system/etc/%s", FontUtils.FALLBACK_FONTS_XML), true);
        RootUtils.runCommand(String.format("busybox chmod 666 /system/etc/%s", FontUtils.SYSTEM_FONTS_XML), true);
        RootUtils.runCommand("busybox reboot -f", true);
    }

}
