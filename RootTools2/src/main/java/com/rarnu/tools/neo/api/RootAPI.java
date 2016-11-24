package com.rarnu.tools.neo.api;

import android.content.Context;
import android.util.Log;
import com.rarnu.tools.neo.utils.FileUtils;
import com.rarnu.tools.neo.utils.RootUtils;
import com.rarnu.tools.neo.xposed.ads.FuckAdSolution;

import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by rarnu on 11/21/16.
 */
public class RootAPI {

    public static boolean isRejected = false;
    public static boolean isSystemRW = false;

    private static String _DU_CMD = "";

    public static boolean mount() {
        String cmd = "mount -o remount,rw /system";
        boolean b = true;
        RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        if (!ret.error.equals("") && ret.error.toLowerCase().contains("denied")) {
            b = false;
        }
        return b;
    }

    public static boolean isSystemRW() {
        // isSystemRW
        boolean b = false;
        String cmd = "mount";
        RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        String[] sl = ret.result.split("\n");
        for (String s: sl) {
            if (s.contains(" /system") && s.contains("ext4")) {
                if (s.contains("rw")) {
                    b = true;
                    break;
                }
            }
        }
        return b;
    }

    public static void makePreferenceReadable(int sdk, String packageName) {
        // makePreferenceReadable
        if (sdk >= 24) {
            String cmd = String.format("chmod -R 777 /data/data/%s/shared_prefs", packageName);
            RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
            Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        }
    }

    public static boolean freezeApplication(String packageName, boolean isFreezed) {
        // freezeApplication
        String cmd = String.format("pm %s %s", (isFreezed ? "disable" : "enable"), packageName);
        RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        return ret.error.equals("");
    }

    public static boolean freezeComponent(String packageName, String componentName, boolean isFreezed) {
        // freezeComponent
        String cmd = String.format("pm %s %s/%s", (isFreezed ? "disable" : "enable"), packageName, componentName);
        RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        return ret.error.equals("");
    }

    public static boolean freezeComponents(String packageName, String[] componentNames, boolean isFreezed) {
        // freezeComponents
        boolean b = true;
        for (int i = 0; i < componentNames.length; i++) {
            String cmd = String.format("pm %s %s/%s", (isFreezed ? "disable" : "enable"), packageName, componentNames[i]);
            RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
            Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
            if (!ret.error.equals("")) {
                b = false;
            }
        }
        return b;
    }

    public static void systemClean(Context ctx) {
        // systemClean
        long totalSize = 0L;
        if (_DU_CMD.equals("")) {
            _DU_CMD = getDuCmd();
        }
        totalSize += cleanCache(ctx);
        totalSize += cleanANR(ctx);
        totalSize += cleanART(ctx);
        cleanCallback(ctx, DeviceAPI.STATUS_COMPLETE, String.format("Total Cleaned: %s", getReadableFileSize(totalSize)));
    }

    public static boolean writeFile(Context ctx, String filePath, String text, int perm) {
        // writeFile
        final String CACHE = "/sdcard/.tmp/";
        boolean b = false;
        File fCache = new File(CACHE);
        if (!fCache.exists()) {
            fCache.mkdirs();
        }
        String tmpPath = CACHE + filePath.substring(filePath.lastIndexOf("/") + 1);
        String tmpPathEx = filePath + ".tmp";
        try {
            FileUtils.rewriteFile(tmpPath, text);
        } catch (Exception e) {

        }
        String modStr = String.valueOf(perm);
        while (modStr.length() < 3) {
            modStr = "0" + modStr;
        }
        File fTmp = new File(tmpPath);
        if (fTmp.exists()) {
            RootUtils.CommandResult ret = RootUtils.runCommand(String.format("cp %s %s", tmpPath, tmpPathEx), true);
            Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
            if (ret.error.equals("")) {
                ret = RootUtils.runCommand(String.format("chmod %s %s", modStr, tmpPathEx), true);
                Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
            }
            if (ret.error.equals("")) {
                ret = RootUtils.runCommand(String.format("cp %s %s", tmpPathEx, filePath), true);
                Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
            }
            if (ret.error.equals("")) {
                ret = RootUtils.runCommand(String.format("chmod %s %s", modStr, filePath), true);
                Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
            }
            b = ret.error.equals("");
        }
        return b;
    }

    public static boolean catFile(String src, String dest, int perm) {
        // catFile
        String modstr = String.valueOf(perm);
        while (modstr.length() < 3) {
            modstr = "0" + modstr;
        }
        RootUtils.CommandResult ret = RootUtils.runCommand(String.format("cat %s > %s", src, dest), true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        if (ret.error.equals("")) {
            ret = RootUtils.runCommand(String.format("chmod %s %s", modstr, dest), true);
            Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        }
        return ret.error.equals("");
    }

    public static void forceDeleteFile(String path) {
        // forceDeleteFile
        String cmd = String.format("rm -f -r %s", path);
        RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
    }

    public static void forceDropCache() {
        // forceDropCache
        RootUtils.CommandResult ret = RootUtils.runCommand("sync", true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        ret = RootUtils.runCommand("echo 3 > /proc/sys/vm/drop_caches", true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        ret = RootUtils.runCommand("echo 0 > /proc/sys/vm/drop_caches", true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
    }

    public static void killProcess() {
        final String CMD_PS = "ps";
        final String CMD_KILL = "kill %s";
        // killProcess
        RootUtils.CommandResult ret = RootUtils.runCommand(CMD_PS, true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        if (ret.error.equals("")) {
            List<String> slPid = new ArrayList<>();
            String[] slPs = ret.result.split("\n");
            for (String s: slPs) {
                if (s.startsWith("u0")) {
                    String pkgName = getPackageName(s);
                    if (!pkgName.contains("core")) {
                        String pidstr = getProcessId(s);
                        if (!pidstr.equals("")) {
                            slPid.add(pidstr);
                        }
                    }
                }
            }
            for (String s: slPid) {
                ret = RootUtils.runCommand(String.format(CMD_KILL, s), true);
                Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
            }
        }
    }

    public static boolean deleteSystemApp(String pkgName) {
        // deleteSystemApp
        boolean b = false;
        RootUtils.CommandResult ret = RootUtils.runCommand(String.format("pm path %s", pkgName), true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        String outstr = ret.result;
        if (!ret.error.equals("") || outstr.trim().equals("")) {
            return b;
        }
        outstr = outstr.replace("package:", "").trim();
        String apkPath = outstr;
        int sc = getSlashCount(apkPath);
        if (sc == 4) {
            String parentDir = apkPath.substring(0, apkPath.lastIndexOf("/"));
            ret = RootUtils.runCommand(String.format("rm -r %s", parentDir), true);
            Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        } else if (sc == 3) {
            ret = RootUtils.runCommand(String.format("rm %s", apkPath), true);
            Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        }
        return ret.error.equals("");
    }

    public static boolean isAppRequiredBySystem(String pkgName) {
        // isAppRequiredBySystem
        boolean b = false;
        RootUtils.CommandResult ret = RootUtils.runCommand(String.format("pm path %s", pkgName), true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        String outstr = ret.result;
        if (!ret.error.equals("") || outstr.trim().equals("")) {
            return b;
        }
        outstr = outstr.replace("package:", "").trim();
        String apkPath = outstr;
        if (apkPath.contains("/data/app") || (apkPath.contains("/data/priv-app"))) {
            return b;
        }
        if (pkgName.equals("android") || pkgName.startsWith("android.") || pkgName.startsWith("com.android.") || (pkgName.startsWith("com.") && pkgName.contains(".android."))) {
            b = true;
        }
        if (pkgName.endsWith(".core") || pkgName.endsWith(".securitycore")) {
            b = true;
        }
        return b;
    }

    private static int getSlashCount(String path) {
        int c = 0;
        for (int i = 0; i< path.length(); i++) {
            if (path.charAt(i) == '/') {
                c++;
            }
        }
        return c;
    }

    private static String getProcessId(String str) {
        if (str.contains("com.rarnu.tools.neo") || str.endsWith(" su")) {
            return "";
        }
        String s =str.trim();
        int  p = s.indexOf(" ");
        s = s.substring(p).trim();
        p = s.indexOf(" ");
        return s.substring(0, p - 1);
    }

    private static String getPackageName(String str) {
        int p = str.lastIndexOf(" ");
        return str.substring(p + 1).trim();
    }

    static class CacheSize {
        long size;
        String sizeReadable;
        CacheSize(String sr, long s) {
            this.size = s;
            this.sizeReadable = sr;
        }
    }

    private static String getDuCmd() {
        final String[] BUSYBOX_PATH = new String[] { "/system/bin/busybox", "/system/xbin/busybox"};
        final String[] DU_PATH = new String[] { "/system/bin/du", "/system/xbin/du" };
        boolean duExists = false;
        boolean busyboxExists = false;
        for (String s: DU_PATH) {
            if (new File(s).exists()) {
                duExists = true;
                break;
            }
        }
        if (duExists) {
            return "du";
        }
        for (String s: BUSYBOX_PATH) {
            if (new File(s).exists()) {
                busyboxExists = true;
                break;
            }
        }
        if (busyboxExists) {
            return "busybox du";
        }
        return "";
    }

    private static CacheSize getCacheSize(String path) {
        String CMD_SIZE_FMT = "%s -s -k \"%s\"";
        String cmd = String.format(CMD_SIZE_FMT, _DU_CMD, path);
        RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
        String sizeStr = "0";
        long size = 0L;
        if (ret.error.equals("")) {
            sizeStr = ret.result.substring(0, ret.result.indexOf("\t")).trim();
            size = Integer.parseInt(sizeStr);
        }
        return new CacheSize(sizeStr, size);
    }

    private static String getReadableFileSize(long size) {
        final String[] UNITS = new String[] { "K", "M", "G", "T", "P" };
        final double MOOD = 1024.0;
        double nSize = size * 1.0;
        int i = 0;
        while (nSize >= MOOD) {
            nSize /= MOOD;
            i++;
        }
        return new DecimalFormat("#.##").format(nSize) + UNITS[i];
    }

    private static boolean deleteCache(String path) {
        final String CMD_DELETE_FMT = "rm -r \"%s\"";
        String cmd = String.format(CMD_DELETE_FMT, path);
        RootUtils.CommandResult ret = RootUtils.runCommand(cmd, true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        return ret.error.equals("");
    }

    private static boolean deleteAnrLog() {
        final String CMD_DELETE_ANR = "rm -r /data/anr/*";
        RootUtils.CommandResult ret = RootUtils.runCommand(CMD_DELETE_ANR, true);
        Log.e("RootAPI", String.format("result: %s, error: %s", ret.result, ret.error));
        return ret.error.equals("");
    }

    private static boolean isCachedAppInstalled(String[] oriList, String app) {
        if (app.startsWith("system") || app.startsWith("data@dalvik-cache")) {
            return true;
        }
        String newAppPath = app.replace("data@app@", "");
        newAppPath = newAppPath.substring(0, newAppPath.indexOf("@"));

        int idx = -1;
        for (int i = 0; i < oriList.length; i++) {
            if (oriList[i].equals(newAppPath)) {
                idx = i;
                break;
            }
        }
        return idx != -1;
    }

    private static boolean isProfileInstalled(String[] oriList, String app) {
        boolean b = false;
        for (String s: oriList) {
            if (s.contains(app)) {
                b = true;
                break;
            }
        }
        return b;
    }

    private static long cleanANR(Context ctx) {
        long l = 0L;
        CacheSize anrSize =getCacheSize("/data/anr/");
        if (deleteAnrLog()) {
            cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Clean ANR (%s)", anrSize.sizeReadable));
            l = anrSize.size;
        }
        return l;
    }

    private static long deleteRemainArtCache(Context ctx) {
        final String CMD_LS_APP = "ls /data/app";
        final String CMD_LS_PKG = "pm list packages";
        final String CMD_LS_ARM = "ls /data/dalvik-cache/arm";
        final String CMD_LS_ARM64 = "ls /data/dalvik-cache/arm64";
        final String CMD_LS_PROFILE = "ls /data/dalvik-cache/profiles";

        RootUtils.CommandResult ret = RootUtils.runCommand(CMD_LS_APP, true);
        String[] listInstalled = ret.result.split("\n");
        ret = RootUtils.runCommand(CMD_LS_PKG, true);
        String[] listAllInstalled = ret.result.split("\n");
        ret = RootUtils.runCommand(CMD_LS_ARM, true);
        String[] listArm = ret.result.split("\n");
        ret = RootUtils.runCommand(CMD_LS_ARM64, true);
        String[] listArm64 = ret.result.split("\n");
        ret = RootUtils.runCommand(CMD_LS_PROFILE, true);
        String[] listProfile = ret.result.split("\n");

        long totalSize =0L;
        for (String s: listArm) {
            if (!s.trim().equals("")) {
                cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Scan %s", s));
                if (!isCachedAppInstalled(listInstalled, s)) {
                    String tmpPath = "/data/dalvik-cache/arm/" + s;
                    CacheSize size = getCacheSize(tmpPath);
                    if (deleteCache(tmpPath)) {
                        cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Clean %s(%s)", s, size.sizeReadable));
                        totalSize += size.size;
                    }
                }
            }
        }
        for (String s: listArm64) {
            if (!s.trim().equals("")) {
                cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Scan %s", s));
                if (!isCachedAppInstalled(listInstalled, s)) {
                    String tmpPath = "/data/dalvik-cache/arm64/" + s;
                    CacheSize size = getCacheSize(tmpPath);
                    if (deleteCache(tmpPath)) {
                        cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Clean %s(%s)", s, size.sizeReadable));
                        totalSize += size.size;
                    }
                }
            }
        }
        for (String s: listProfile) {
            if (!s.trim().equals("")) {
                cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Scan %s", s));
                if (!isProfileInstalled(listAllInstalled, s)) {
                    String tmpPath = "/data/dalvik-cache/profiles/" + s;
                    CacheSize size = getCacheSize(tmpPath);
                    if (deleteCache(tmpPath)) {
                        cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Clean %s(%s)", s, size.sizeReadable));
                        totalSize += size.size;
                    }
                }
            }
        }
        return totalSize;
    }

    private static long cleanCache(Context ctx) {
        long totalSize = 0L;
        final String CMD_FIND_CACHE = "find /data/data/ -type dir -name \"cache\"";
        RootUtils.CommandResult ret = RootUtils.runCommand(CMD_FIND_CACHE,true);
        if (!ret.error.equals("")) {
            cleanCallback(ctx, DeviceAPI.STATUS_ERROR, "Can not clean Cache");
            return 0;
        }
        String[] items = ret.result.split("\n");
        for (String s: items) {
            cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Scan %s", s));
            CacheSize cs = getCacheSize(s);
            if (cs.size > 16) {
                if (deleteCache(s)) {
                    cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, String.format("Clean %s(%s)", s, cs.sizeReadable));
                    totalSize += cs.size;
                }
            }
        }
        return totalSize;
    }

    private static long cleanART(Context ctx) {
        return deleteRemainArtCache(ctx);
    }

    public static void cleanCallback(Context ctx, int status, String data) {
        DeviceAPI.cleanCallback(ctx, status, data);
    }


}
