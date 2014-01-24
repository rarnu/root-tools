package com.rarnu.fontter;

import android.content.Context;
import android.os.Environment;
import com.rarnu.command.RootUtils;
import com.rarnu.fontter.adapter.FontItem;
import com.rarnu.utils.FileUtils;

import java.io.File;
import java.io.IOException;

public class FontInstaller {

    private static String tmpDir = Environment.getExternalStorageDirectory().getAbsolutePath() + "/.font_tmp/";
    private static String[] FONT_FILES = new String[]{
            "DroidSans.ttf",
            "DroidSansFallback-htc.ttf",
            "DroidSansFallback.ttf",
            "DroidSerif-Regular.ttf",
            "MTLmr3m.ttf",
            "DroidSansMono.ttf",
            "HelveticaNeueLTPro-Cn.otf",
            "gcsh00d-hkscs.ttf",
            "HelveticaNeueOTS.ttf",
            "NanumGothic.ttf",
            "RobotoCondensed-Regular.ttf",
            "Roboto-Regular.ttf"
    };

    public static void installFont(final Context context, final FontItem item) {
        makeTmpDir();
        copyFiles(context, item);
        installFiles();
        removeTmpDir();
        rebootDevice();
    }

    private static void makeTmpDir() {

        File fTmpDir = new File(tmpDir);
        if (!fTmpDir.exists()) {
            fTmpDir.mkdirs();
        }
    }

    private static void removeTmpDir() {
        FileUtils.deleteDir(tmpDir);
    }

    private static void copyFiles(final Context context, final FontItem item) {
        String fontPath = tmpDir + item.fileName;
        FileUtils.copyAssetFile(context, item.fileName, tmpDir, null);
        File fNewFont = new File(fontPath);
        if (fNewFont.exists()) {
            for (String s : FONT_FILES) {
                try {
                    FileUtils.copyFile(fontPath, tmpDir + s, null);
                } catch (IOException e) {

                }
            }
        }
    }

    private static void installFiles() {
        for (String s : FONT_FILES) {
            RootUtils.runCommand(String.format("busybox cp -f %s /system/fonts/", tmpDir + s), true);
            RootUtils.runCommand(String.format("busybox chmod 666 /system/fonts/%s", s), true);
        }
    }

    private static void rebootDevice() {
        RootUtils.runCommand("busybox reboot -f", true);
    }

}
