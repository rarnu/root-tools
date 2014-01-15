package com.rarnu.tools.root.utils;

import android.app.ActivityManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.*;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.AssetManager;
import android.content.res.Resources;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Handler;
import android.os.Message;
import android.util.DisplayMetrics;
import com.rarnu.command.CommandCallback;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.common.SysappInfo;

import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

public class ApkUtils {

    public static final int INSTALL_AUTO = 0;
    public static final int INSTALL_INTERNAL = 1;
    public static final int INSTALL_SDCARD = 2;
    private static final String PACKAGE_URL = "http://rarnu.7thgen.info/root_tools/package/";

    public static List<String> getLauncherPackageName(Context context) {
        List<String> ret = null;
        if (context != null) {
            Intent inLauncher = new Intent(Intent.ACTION_MAIN);
            inLauncher.addCategory(Intent.CATEGORY_HOME);
            List<ResolveInfo> list = context.getPackageManager().queryIntentActivities(inLauncher, 0);

            if (list != null && list.size() != 0) {
                ret = new ArrayList<String>();
                for (ResolveInfo ri : list) {
                    if (ri.activityInfo != null) {
                        ret.add(ri.activityInfo.packageName);
                    }
                }
            }
        }
        return ret;

    }

    public static String getTopPackage(Context context) {
        ActivityManager am = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        String ret = "";
        try {
            ComponentName cn = am.getRunningTasks(1).get(0).topActivity;
            ret = cn.getPackageName();
        } catch (Exception e) {

        }
        return ret;
    }

    public static List<SysappInfo> getSystemApps(Context context) {
        List<SysappInfo> res = new ArrayList<SysappInfo>();
        if (context != null) {
            List<PackageInfo> packs = null;
            try {
                packs = context.getPackageManager().getInstalledPackages(0);
            } catch (Exception e) {

            }

            int position = 0;
            if (packs != null & packs.size() != 0) {
                for (PackageInfo p : packs) {

                    ApplicationInfo newInfo = p.applicationInfo;
                    if (newInfo == null) {
                        continue;
                    }
                    if (newInfo.sourceDir.contains("/system/app")) {
                        SysappInfo info = new SysappInfo();
                        info.info = newInfo;
                        info.level = getAppLevel(newInfo.sourceDir, newInfo.packageName);
                        info.position = position;
                        res.add(info);
                        position++;
                    }
                }
            }
        }
        return res;
    }

    public static String getAppSize(String path) {
        long fileLen = getFileSize(path);
        String odexPath = path.substring(0, path.length() - 3) + "odex";
        File fOdex = new File(odexPath);
        if (fOdex.exists()) {
            fileLen += getFileSize(odexPath);
        }
        return new DecimalFormat("#.##").format(fileLen / 1024);
    }

    private static long getFileSize(String path) {
        File f = new File(path);
        return f.length();
    }

    public static String getDataSize(String path) {
        String ret = "";
        CommandResult result = RootUtils.runCommand("busybox du -s " + path, true, null);
        if (result.error.equals("")) {
            ret = result.result;
            try {
                ret = ret.substring(0, ret.indexOf('\t'));
            } catch (Exception e) {
                ret = "unknown";
            }
        }
        return ret;
    }

    public static boolean backupSystemApp(String path) {
        String fn = path.substring(0, path.length() - 3) + "*";
        CommandResult result = RootUtils.runCommand("busybox cp " + fn + " " + DirHelper.SYSAPP_DIR, true, null);
        return result.error.equals("");
    }

    public static boolean deleteSystemApp(String path) {
        String fn = path.substring(0, path.length() - 3) + "*";
        CommandResult result = RootUtils.runCommand("rm " + fn, true, null);
        return result.error.equals("");
    }

    public static boolean deleteSystemAppData(String ns) {
        CommandResult result = RootUtils.runCommand("rm -r " + ns, true, null);
        return result.error.equals("");
    }

    public static boolean installSystemApp(String path) {
        String fn = path.substring(0, path.length() - 3) + "*";
        String onlyApkName = path.substring(path.lastIndexOf("/") + 1);
        CommandResult result = RootUtils.runCommand("busybox cp -r " + fn + " /system/app/", true, null);
        if (result.error.equals("")) {
            result = RootUtils.runCommand("chmod 644 /system/app/" + onlyApkName, true, null);
        }
        return result.error.equals("");
    }

    public static boolean installApp(DataappInfo info) {
        return installApp(info.localPath);
    }

    public static boolean installApp(String filePath) {
        CommandResult result = RootUtils.runCommand("pm install -r " + filePath, true);
        return result.result.toLowerCase().contains("success");
    }

    public static String installAppWithResult(String filePath) {
        CommandResult result = RootUtils.runCommand("pm install -r " + filePath, true);
        return result.error;
    }

    public static String forceInstallAppWithResult(String filePath) {
        // change to package name
        ApplicationInfo appInfo = getAppInfoFromPackage(filePath);
        String fileName = appInfo.packageName + "-1.apk";
        // force install

        CommandResult result = RootUtils.runCommand(String.format("busybox cp %s /data/app/%s", filePath, fileName), true);
        if (result.error.equals("")) {
            result = RootUtils.runCommand(String.format("chmod 644 /data/app/%s", fileName), true);
        }
        if (result.error.equals("")) {
            result = RootUtils.runCommand(String.format("chown system:system /data/app/%s", fileName), true);
        }
        return result.error;
    }

    public static boolean isAndroidApp(String path) {
        boolean ret = false;
        for (String s : AppNameConst.systemApps) {
            if (path.toLowerCase().equals(s.toLowerCase())) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    public static boolean isGoogleApp(String ns) {
        return ns.contains("com.google.");
    }

    public static boolean isHtcApp(String ns) {
        return ns.contains("com.htc.");
    }

    public static int getAppLevel(String path, String ns) {
        File fApk = new File(path);
        String apkName = fApk.getName();

        int applevel = 3;
        if (ApkUtils.isAndroidApp(apkName)) {
            applevel = 0;
        }
        if (applevel == 3) {
            if (ApkUtils.isGoogleApp(ns)) {
                applevel = 1;
            }
        }
        if (applevel == 3) {
            if (ApkUtils.isHtcApp(ns)) {
                applevel = 2;
            }
        }
        return applevel;
    }

    public static Drawable getIconFromPackage(Context context, String archiveFilePath) {
        PackageParser packageParser = new PackageParser(archiveFilePath);
        File sourceFile = new File(archiveFilePath);
        DisplayMetrics metrics = new DisplayMetrics();
        metrics.setToDefaults();
        PackageParser.Package pkg = packageParser.parsePackage(sourceFile, archiveFilePath, metrics, 0);
        if (pkg == null) {
            return context.getResources().getDrawable(R.drawable.android);
        }
        ApplicationInfo info = pkg.applicationInfo;

        Resources pRes = context.getResources();
        AssetManager assmgr = new AssetManager();
        assmgr.addAssetPath(archiveFilePath);
        Resources res = new Resources(assmgr, pRes.getDisplayMetrics(), pRes.getConfiguration());

        if (info.icon != 0) {
            Drawable icon = res.getDrawable(info.icon);
            return icon;
        } else {
            return context.getResources().getDrawable(R.drawable.android);
        }
    }

    public static void installSystemApp(final Context context, final String path, final Handler h) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean installOK = installSystemApp(path);
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e) {
                }
                Message msg = new Message();
                msg.what = 1;
                msg.arg1 = (installOK ? 1 : 0);
                h.sendMessage(msg);
            }
        }).start();
    }

    public static String getLabelFromPackage(Context context, ApplicationInfo info, String path) {
        return getLabelFromPackageFile(context, info, path + info.packageName + ".apk");
    }

    public static String getLabelFromPackage(Context context, ApplicationInfo info) {
        return getLabelFromPackageFile(context, info, DirHelper.DATAAPP_DIR + info.packageName + ".apk");
    }

    public static String getLabelFromPackageFile(Context context, ApplicationInfo info, String fileName) {
        Resources res = context.getResources();
        AssetManager assetMag = new AssetManager();
        assetMag.addAssetPath(fileName);
        res = new Resources(assetMag, res.getDisplayMetrics(), res.getConfiguration());
        try {
            if (info.labelRes != 0) {
                return res.getText(info.labelRes).toString();
            } else {
                return context.getResources().getString(R.string.no_name);
            }
        } catch (Exception e) {
            return context.getResources().getString(R.string.no_name);
        }
    }

    public static Drawable getIconFromPackage(Context context, ApplicationInfo info, String path) {
        return getIconFromPackageFile(context, info, path + info.packageName + ".apk");
    }

    public static Drawable getIconFromPackage(Context context, ApplicationInfo info) {
        return getIconFromPackageFile(context, info, DirHelper.DATAAPP_DIR + info.packageName + ".apk");
    }

    public static Drawable getIconFromPackageFile(Context context, ApplicationInfo info, String fileName) {

        Resources res = context.getResources();
        if (info == null) {
            return res.getDrawable(android.R.drawable.sym_def_app_icon);
        }
        AssetManager assmgr = new AssetManager();
        assmgr.addAssetPath(fileName);
        res = new Resources(assmgr, res.getDisplayMetrics(), res.getConfiguration());
        try {
            if (info.icon != 0) {
                return res.getDrawable(info.icon);
            } else {
                return res.getDrawable(android.R.drawable.sym_def_app_icon);
            }
        } catch (Exception e) {
            return res.getDrawable(android.R.drawable.sym_def_app_icon);
        }
    }

    public static String getLabelFromPackageEx(Context context, ApplicationInfo info) {
        Resources res = context.getResources();
        AssetManager assetMag = new AssetManager();
        assetMag.addAssetPath(info.publicSourceDir);
        res = new Resources(assetMag, res.getDisplayMetrics(), res.getConfiguration());
        if (info.labelRes != 0) {
            return res.getText(info.labelRes).toString();
        } else {
            return info.packageName;
        }
    }

    public static ApplicationInfo getAppInfoFromPackage(String filePath) {
        ApplicationInfo info = null;
        PackageParser.Package pkg = getPackageInfoFromPackage(filePath, false);
        if (pkg != null) {
            info = pkg.applicationInfo;
        }
        return info;
    }

    public static PackageParser.Package getPackageInfoFromPackage(String filePath, boolean collectSignature) {
        PackageParser packageParser = new PackageParser(filePath);
        File sourceFile = new File(filePath);
        DisplayMetrics metrics = new DisplayMetrics();
        metrics.setToDefaults();
        PackageParser.Package pkg = packageParser.parsePackage(sourceFile, filePath, metrics, 0);
        if (pkg != null && collectSignature) {
            packageParser.collectCertificates(pkg, 0);
        }

        return pkg;
    }

    public static List<DataappInfo> getInstalledApps(Context context, boolean includeSystem) {
        List<DataappInfo> res = new ArrayList<DataappInfo>();
        if (context != null) {
            List<PackageInfo> packs = null;
            try {
                packs = context.getPackageManager().getInstalledPackages(0);
            } catch (Exception e) {

            }
            int position = 0;
            if (packs != null && packs.size() != 0) {
                for (PackageInfo p : packs) {
                    ApplicationInfo newInfo = p.applicationInfo;
                    if (newInfo == null) {
                        continue;
                    }
                    if ((includeSystem && newInfo.sourceDir.contains("/system/app/")) || newInfo.sourceDir.contains("/data/app/")) {
                        DataappInfo info = new DataappInfo();
                        info.info = newInfo;
                        info.checked = false;
                        info.position = position;
                        info.installed = false;
                        res.add(info);
                        position++;
                    }
                }
            }
        }
        return res;
    }

    public static List<EnableappInfo> getInstalledAppsEnabled(Context context) {
        List<EnableappInfo> res = new ArrayList<EnableappInfo>();
        if (context != null) {
            List<PackageInfo> packs = null;
            try {
                packs = context.getPackageManager().getInstalledPackages(0);
            } catch (Exception e) {

            }
            if (packs != null && packs.size() != 0) {
                for (PackageInfo p : packs) {

                    ApplicationInfo newInfo = p.applicationInfo;
                    if (newInfo == null) {
                        continue;
                    }

                    EnableappInfo info = new EnableappInfo();
                    info.info = newInfo;
                    info.enabled = true;

                    if (newInfo.sourceDir.contains("/system/app/")) {
                        info.type = 0;
                    } else if (newInfo.sourceDir.contains("/data/app/")) {
                        info.type = 1;
                    } else {
                        info.type = 2;
                    }

                    res.add(info);

                }
            }
        }
        return res;
    }

    public static List<EnableappInfo> getInstalledAppsDisabled(Context context) {
        List<EnableappInfo> res = new ArrayList<EnableappInfo>();
        File fDisableSystem = new File(DirHelper.ENABLEAPP_DIR_SYSTEM);
        if (fDisableSystem.exists()) {
            for (String s : fDisableSystem.list()) {
                if (s.toLowerCase().endsWith(".apk")) {
                    EnableappInfo newinfo = new EnableappInfo();
                    newinfo.info = getAppInfoFromPackage(DirHelper.ENABLEAPP_DIR_SYSTEM + s);

                    if (newinfo.info == null) {
                        continue;
                    }
                    newinfo.type = 0;
                    newinfo.enabled = false;
                    newinfo.filePath = DirHelper.ENABLEAPP_DIR_SYSTEM + s;
                    res.add(newinfo);
                }
            }
        }

        File fDisableData = new File(DirHelper.ENABLEAPP_DIR_DATA);
        if (fDisableData.exists()) {
            for (String s : fDisableData.list()) {
                if (s.toLowerCase().endsWith(".apk")) {
                    EnableappInfo newinfo = new EnableappInfo();
                    newinfo.info = getAppInfoFromPackage(DirHelper.ENABLEAPP_DIR_DATA + s);

                    if (newinfo.info == null) {
                        continue;
                    }
                    newinfo.type = 1;
                    newinfo.enabled = false;
                    newinfo.filePath = DirHelper.ENABLEAPP_DIR_DATA + s;
                    res.add(newinfo);
                }
            }
        }

        return res;
    }

    public static List<EnableappInfo> getEnabledApplications(Context context) {
        List<EnableappInfo> listEnabled = getInstalledAppsEnabled(context);
        List<EnableappInfo> listDisabled = getInstalledAppsDisabled(context);
        listDisabled.addAll(listEnabled);
        return listDisabled;
    }

    public static boolean uninstallApk(String packageName) {
        try {
            CommandResult cmdRet = RootUtils.runCommand(String.format("pm uninstall %s", packageName), true, null);
            return cmdRet.error.equals("");
        } catch (Exception e) {
            return false;
        }
    }

    public static boolean applicationInstalled(Context context, String namespace) {
        try {
            PackageInfo info = context.getPackageManager().getPackageInfo(namespace, 0);
            return info != null;
        } catch (NameNotFoundException e) {
            return false;
        }
    }

    public static boolean startApplication(String namespace, String activity) {
        try {
            String cmd = "am start -a android.intent.action.MAIN -c android.intent.category.LAUNCHER -n %s/%s";
            Runtime.getRuntime().exec(String.format(cmd, namespace, activity));
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    public static void gotoApp(Context context, String namespace, String url) {
        if (ApkUtils.applicationInstalled(context, namespace)) {
            openApp(context, namespace);
            // ApkUtils.startApplication(namespace, activity);
        } else {
            openDownloadApp(context, url);
        }
    }

    public static void openDownloadApp(Context context, String url) {
        String downloadUrl = PACKAGE_URL + url;
        Intent inDownload = new Intent(Intent.ACTION_VIEW);
        inDownload.setData(Uri.parse(downloadUrl));
        context.startActivity(inDownload);
    }

    public static void openGooglePlayForApp(Context context, String namespace) {
        Intent inPlay = new Intent(Intent.ACTION_VIEW);
        inPlay.setData(Uri.parse("market://details?id=" + namespace));
        context.startActivity(inPlay);
    }

    public static void setInstallLocation(int location) {
        RootUtils.runCommand("pm set-install-location " + String.valueOf(location), true, null);
    }

    public static boolean openApp(Context context, String packageName) {
        return openApp(context, packageName, false);
    }

    public static boolean openApp(Context context, String packageName, boolean newTask) {
        PackageInfo pi = null;
        try {
            pi = context.getPackageManager().getPackageInfo(packageName, 0);
        } catch (Exception e) {
        }

        if (pi == null) {
            return false;
        }

        Intent resolveIntent = new Intent(Intent.ACTION_MAIN, null);
        resolveIntent.addCategory(Intent.CATEGORY_LAUNCHER);
        resolveIntent.setPackage(pi.packageName);

        boolean ret = false;
        if (context != null) {
            List<ResolveInfo> apps = context.getPackageManager().queryIntentActivities(resolveIntent, 0);
            try {
                ResolveInfo ri = apps.iterator().next();
                if (ri != null) {
                    String className = ri.activityInfo.name;
                    Intent intent = new Intent(Intent.ACTION_MAIN);
                    intent.addCategory(Intent.CATEGORY_LAUNCHER);
                    ComponentName cn = new ComponentName(packageName, className);
                    intent.setComponent(cn);
                    if (newTask) {
                        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    }
                    context.startActivity(intent);
                    ret = true;
                }
            } catch (Exception e) {

            }
        }
        return ret;
    }

    public static void scanApksInSdcard(final CommandCallback callback) {
        new Thread(new Runnable() {

            @Override
            public void run() {
                String cmd = "busybox find /sdcard/ -name \"*.apk\"";
                RootUtils.runCommand(cmd, true, callback);
            }
        }).start();
    }

    public static boolean isAppInstalled(Context context, String packageName) {
        ApplicationInfo info = null;
        try {
            info = context.getPackageManager().getApplicationInfo(packageName, 0);
        } catch (NameNotFoundException e) {

        }
        return info != null;
    }

    /**
     * getApkFileStatus
     *
     * @param newinfo
     * @return status with the new application info<br>
     * return 0: installed with same signature<br>
     * return 1: installed with different signature<br>
     * return 2: no need update<br>
     * return 3: not installed<br>
     * return 4: error
     */
    public static int getApkFileStatus(Context context, DataappInfo newinfo) {
        try {
            String packageName = newinfo.info.packageName;
            ApplicationInfo installedInfo = null;
            try {
                installedInfo = context.getPackageManager().getApplicationInfo(packageName, 0);
            } catch (NameNotFoundException e) {

            }
            if (installedInfo == null) {
                return 3;
            }
            int newVer = DeviceUtils.getAppVersionCode(context, newinfo.localPath);
            int oldVer = DeviceUtils.getAppVersionCode(context, installedInfo);
            if (newVer <= oldVer) {
                return 2;
            }
            boolean compare = SignatureUtils.compareSignature(newinfo.localPath, installedInfo.publicSourceDir);
            // int compare = GlobalInstance.pm.checkSignatures(newinfo.info.uid, installedInfo.uid);
            return (compare ? 0 : 1);
        } catch (Exception e) {
            return 4;
        }
    }

    public static boolean checkSignature(String apk1, String apk2) {
        // check signature
        List<String> sig1 = null;
        try {
            sig1 = SignatureUtils.getSignaturesFromApk(apk1);
        } catch (IOException e) {
        }
        List<String> sig2 = null;
        try {
            sig2 = SignatureUtils.getSignaturesFromApk(apk2);
        } catch (IOException e) {
        }
        return sig1.equals(sig2);
    }

    public static ApplicationInfo findApplication(Context context, String regex, boolean system) {
        ApplicationInfo info = null;
        if (context != null) {
            String[] regs = regex.split("\\|"); // 0:start, 1:end, 2:contain
            PackageManager pm = context.getPackageManager();
            List<ApplicationInfo> list = null;
            try {
                list = pm.getInstalledApplications(0);
            } catch (Exception e) {

            }
            boolean match = true;
            if (list != null && list.size() != 0) {
                String pkgName = "";
                for (ApplicationInfo pn : list) {
                    if (system && !pn.publicSourceDir.contains("/system/app")) {
                        continue;
                    }
                    pkgName = pn.packageName;
                    if (!regs[0].trim().equals("")) {
                        match = pkgName.startsWith(regs[0].trim());
                    }
                    if (match && !regs[1].trim().equals("")) {
                        match = pkgName.endsWith(regs[1].trim());
                    }
                    if (match && !regs[2].trim().equals("")) {
                        match = pkgName.contains(regs[2].trim());
                    }
                    if (match) {
                        try {
                            info = pm.getApplicationInfo(pkgName, 0);
                        } catch (Exception e) {

                        }
                        if (info != null) {
                            break;
                        }
                    }
                }
            }
        }
        return info;
    }

    public static void openInstallApk(Context context, String apkPath) {
        Intent intent = new Intent(Intent.ACTION_VIEW);
        intent.setDataAndType(Uri.parse("file://" + apkPath), "application/vnd.android.package-archive");
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(intent);
    }
}
