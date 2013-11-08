package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.util.Log;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.DataappInfo;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class BackupRestoreUtils {

    private static List<DataappInfo> operationLog = new ArrayList<DataappInfo>();

    public static void clearOperationLog() {
        operationLog.clear();
    }

    public static List<DataappInfo> getOperationLog() {
        return operationLog;
    }

    public static List<DataappInfo> getBackupedApps(Context context, String path) {
        List<DataappInfo> res = new ArrayList<DataappInfo>();
        File fBackupDir = new File(path);
        int position = 0;
        if (fBackupDir.exists()) {
            for (String s : fBackupDir.list()) {
                if (s.toLowerCase().endsWith(".apk")) {
                    DataappInfo newinfo = new DataappInfo();
                    newinfo.info = ApkUtils.getAppInfoFromPackage(path + s);
                    newinfo.checked = false;
                    newinfo.position = position;
                    if (newinfo.info == null) {
                        continue;
                    }
                    res.add(newinfo);
                    position++;
                }
            }
        }
        return res;
    }

    public static List<DataappInfo> getBackupedApps(Context context) {
        return getBackupedApps(context, DirHelper.DATAAPP_DIR);
    }

    public static void backupData(Context context, String apk, String packageName, String savePath, DataappInfo info) {

        if (savePath == null) {
            savePath = DirHelper.DATAAPP_DIR;
        }
        info.type = 1;
        String apkName = String.format(savePath + "%s.apk", packageName);
        File apkFile = new File(apkName);
        if (apkFile.exists()) {
            if (!GlobalInstance.overrideBackuped) {
                info.log = context.getResources().getString(R.string.backup_exists);
                info.logId = 1;
                operationLog.add(info);
                return;
            } else {
                String delCmd = String.format("rm -r " + savePath + "%s*", packageName);
                RootUtils.runCommand(delCmd, true, null);
            }
        }

        // delete cache before backup
        String cmd = String.format("rm -r /data/data/%s/cache", packageName);
        RootUtils.runCommand(cmd, true, null);

        cmd = String.format("busybox cp -r /data/data/%s " + savePath, packageName);
        CommandResult result = RootUtils.runCommand(cmd, true, null);

        cmd = String.format("busybox find " + savePath + "%s/ -name \"cache\" | busybox xargs rm -r", packageName);
        RootUtils.runCommand(cmd, true, null);
        cmd = String.format("busybox find " + savePath + "%s/ -name \"lib\" | busybox xargs rm -r", packageName);
        RootUtils.runCommand(cmd, true, null);
        cmd = String.format("busybox find " + savePath + "%s/ -name \"webview*\" | busybox xargs rm -r", packageName);
        RootUtils.runCommand(cmd, true, null);
        cmd = String.format("busybox cp %s " + savePath + "%s.apk", apk, packageName);
        result = RootUtils.runCommand(cmd, true, null);

        if (result.error.equals("")) {
            // backup data under sdcard
            backupSdData(context, packageName, savePath);
            backupObbData(context, packageName, savePath);
            info.log = context.getResources().getString(R.string.backup_ok);
            info.logId = 0;
            operationLog.add(info);

        } else {
            info.log = context.getResources().getString(R.string.backup_fail);
            info.logId = 2;
            operationLog.add(info);
        }
    }

    public static void backupData(Context context, String apk, String path, DataappInfo info) {
        backupData(context, apk, path, null, info);
    }

    public static void backupSdData(Context context, String packageName, String savePath) {
        // /sdcard/Android/data/<packageName>
        String sdDataPath = DirHelper.DATA_SDCARD_DIR + packageName;
        if (new File(sdDataPath).exists()) {
            RootUtils.runCommand(String.format("busybox cp -r \"%s\" \"%s%s_sddata\"", sdDataPath, savePath, packageName), true);

        }
    }

    public static void backupObbData(Context context, String packageName, String savePath) {
        // /sdcard/Android/obb/<packageName>
        String sdObbPath = DirHelper.OBB_SDCARD_DIR + packageName;
        if (new File(sdObbPath).exists()) {
            RootUtils.runCommand(String.format("busybox cp -r \"%s\" \"%s%s_sdobb\"", sdObbPath, savePath, packageName), true);
        }
    }

    public static void restoreData(Context context, String packageName, String savePath, DataappInfo info) {
        if (savePath == null) {
            savePath = DirHelper.DATAAPP_DIR;
        }
        info.type = 2;
        String cmd = String.format("pm install -r " + savePath + "%s.apk", packageName);
        CommandResult result = null;
        if (GlobalInstance.reinstallApk) {
            try {
                result = RootUtils.runCommand(cmd, true, null);
                Log.e("restoreData", String.format("ret:%s, err:%s", result.result, result.error));
            } catch (Throwable th) {
                result = new CommandResult();
                result.result = "error";
            }
        } else {
            result = new CommandResult();
            result.result = "success";
        }

        if (result.result.toLowerCase().equals("success")) {
            cmd = String.format("busybox cp -r " + savePath + "%s /data/data/", packageName);
            result = RootUtils.runCommand(cmd, true, null);
            Log.e("restoreData", String.format("ret:%s, err:%s", result.result, result.error));
            if (result.error.equals("")) {

                cmd = String.format("busybox chmod -R 777 /data/data/%s/*", packageName);
                result = RootUtils.runCommand(cmd, true, null);
                Log.e("restoreData", String.format("ret:%s, err:%s", result.result, result.error));
                if (result.error.equals("")) {
                    restoreSdData(context, packageName, savePath);
                    restoreObbData(context, packageName, savePath);
                    info.log = context.getResources().getString(R.string.restore_ok);
                    info.logId = 0;
                    operationLog.add(info);
                } else {
                    info.log = context.getResources().getString(R.string.restore_fail);
                    info.logId = 2;
                    operationLog.add(info);
                }
            } else {
                info.log = context.getResources().getString(R.string.restore_fail);
                info.logId = 2;
                operationLog.add(info);
            }
        } else {
            info.log = context.getResources().getString(R.string.restore_fail);
            info.logId = 2;
            operationLog.add(info);
        }
    }

    public static void restoreData(Context context, String packageName, DataappInfo info) {
        restoreData(context, packageName, null, info);
    }

    public static void restoreSdData(Context context, String packageName, String savePath) {
        if (!new File(DirHelper.DATA_SDCARD_DIR).exists()) {
            new File(DirHelper.DATA_SDCARD_DIR).mkdirs();
        }
        String sdDataPath = savePath + packageName + "_sddata";
        if (new File(sdDataPath).exists()) {
            RootUtils.runCommand(String.format("busybox cp -r \"%s\" \"%s%s\"", sdDataPath, DirHelper.DATA_SDCARD_DIR, packageName), true);

        }
    }

    public static void restoreObbData(Context context, String packageName, String savePath) {
        if (!new File(DirHelper.OBB_SDCARD_DIR).exists()) {
            new File(DirHelper.OBB_SDCARD_DIR).mkdirs();
        }
        String sdObbData = savePath + packageName + "_sdobb";
        if (new File(sdObbData).exists()) {
            RootUtils.runCommand(String.format("busybox cp -r \"%s\" \"%s%s\"", sdObbData, DirHelper.OBB_SDCARD_DIR, packageName), true);

        }
    }

    public static void deleteBackupData(String packageName, String path) {
        String cmd = String.format("busybox rm -r " + path + "%s*", packageName);
        RootUtils.runCommand(cmd, true, null);
    }

    public static void deleteBackupData(String packageName) {
        deleteBackupData(packageName, DirHelper.DATAAPP_DIR);
    }

    public static void deleteAllBackupData(String path) {
        RootUtils.runCommand("busybox rm -r " + path + "*", true, null);
    }

    public static void deleteAllBackupData() {
        deleteAllBackupData(DirHelper.DATAAPP_DIR);
    }

    public static boolean forceInstallApp(Context context, DataappInfo info) {
        boolean ret = false;
        try {
            ApplicationInfo newinfo = ApkUtils.getAppInfoFromPackage(info.localPath);
            String packageName = newinfo.packageName;

            backupData(context, info.localPath, packageName, DirHelper.FORCE_UPDATE_DIR, info);
            RootUtils.runCommand("pm uninstall " + packageName, true);
            restoreData(context, packageName, DirHelper.FORCE_UPDATE_DIR, info);
            RootUtils.runCommand("rm -r " + DirHelper.FORCE_UPDATE_DIR + packageName + "*", true);
            ret = true;
        } catch (Exception e) {

        }
        return ret;
    }
}
