package com.rarnu.utils;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.telephony.TelephonyManager;
import android.util.Log;
import android.widget.Toast;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;

import java.io.File;
import java.util.Random;

public class MiscUtils {

    public static void doScanMedia(Context context) {
        context.sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, Uri.parse("file://" + Environment.getExternalStorageDirectory().getAbsolutePath())));
        Toast.makeText(context, R.string.scan, Toast.LENGTH_LONG).show();
    }

    public static boolean isSDCardExists() {
        return (Environment.getExternalStorageState().equals(Environment.MEDIA_MOUNTED));
    }

    public static String getSecondSdcardPath(boolean hasSplit) {
        CommandResult ret = RootUtils.runCommand("mount", false, null);
        String[] lines = ret.result.split("\n");

        String systemSdcard = Environment.getExternalStorageDirectory().getAbsolutePath();
        String extSdcard = "";

        for (String s : lines) {
            if (s.contains("secure") || s.contains("asec")) {
                continue;
            }
            if (s.contains("fat") || s.contains("fuse")) {
                String columns[] = s.split(" ");
                if (columns != null && columns.length > 3) {
                    if (columns[3].contains("rw,")) {
                        extSdcard += columns[1] + "\n";
                    }
                }
            }
        }
        extSdcard = extSdcard.replace(systemSdcard, "").trim();
        if (!extSdcard.endsWith("/") && hasSplit) {
            extSdcard += "/";
        }
        return extSdcard;
    }


    public static boolean isEmulator(Context context) {
        boolean ret = false;
        try {
            TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);
            String imei = tm.getDeviceId();
            if (imei != null && imei.equals("000000000000000")) {
                ret = true;
            }
            ret = (Build.MODEL.equals("sdk")) || (Build.MODEL.equals("google_sdk"));
            if (!ret) {
                ret = isBlueStacks();
            }
            if (!ret) {
                ret = isGenymotion();
            }
        } catch (Exception ioe) {

        }
        return ret;
    }


    public static String randomString() {
        StringBuilder result = new StringBuilder();
        String text = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        char[] c = text.toCharArray();
        Random random = new Random();
        for (int i=0; i<11; i++) {
            result.append(c[random.nextInt(c.length)]);
        }
        return result.toString();
    }

    private static boolean isBlueStacks() {
        return Build.MODEL.toLowerCase().contains("bluestacks");
    }

    private static boolean isGenymotion() {
        return Build.MODEL.toLowerCase().contains("genymotion");
    }

    public static boolean isNoMediaContained(String path) {
        String rootPath = Environment.getExternalStorageDirectory().getAbsolutePath();
        Log.e("isNoMediaContained", "root: "+rootPath);
        String basePath = path;
        boolean hasNomedia = false;
        while (!basePath.equals(rootPath)) {
            if (new File(basePath, ".nomedia").exists()) {
                hasNomedia = true;
                break;
            }

            basePath = basePath.substring(0, basePath.lastIndexOf("/"));
        }
        return hasNomedia;
    }
}
