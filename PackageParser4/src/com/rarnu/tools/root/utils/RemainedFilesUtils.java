package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.RemainedInfo;

import java.util.ArrayList;
import java.util.List;

public class RemainedFilesUtils {
    public static List<RemainedInfo> getRemainedFiles(Context context) {
        // get remained files list
        List<RemainedInfo> list = new ArrayList<RemainedInfo>();
        if (context != null) {
            CommandResult retData = RootUtils.runCommand("ls /data/data/", true);
            CommandResult retLib = RootUtils.runCommand("ls /data/app-lib/", true);
            String[] arrData = null;
            String[] arrLib = null;
            if (retData != null && retData.error.equals("")) {
                arrData = retData.result.split("\n");
            }
            if (retLib != null && retLib.error.equals("")) {
                arrLib = retLib.result.split("\n");
            }
            List<ApplicationInfo> apps = null;
            try {
                apps = context.getPackageManager().getInstalledApplications(0);
            } catch (Exception e) {

            }
            if (apps != null) {
                if (arrData != null && arrData.length != 0) {
                    for (String s : arrData) {
                        if (s == null || s.trim().equals("")) {
                            continue;
                        }
                        if (!isApkInstalled(s, apps)) {
                            RemainedInfo info = new RemainedInfo();
                            info.packageName = s;
                            info.path = "/data/data/" + s;
                            info.dirSize = FileUtils.getDirSize(info.path);
                            try {
                                if (info.dirSize.equals("unknown") || Integer.parseInt(info.dirSize) > 4) {
                                    info.dirSize += "K";
                                    list.add(info);
                                }
                            } catch (Exception e) {

                            }

                        }
                    }
                }
                if (arrLib != null && arrLib.length != 0) {
                    for (String s : arrLib) {
                        if (s == null || s.trim().equals("")) {
                            continue;
                        }
                        if (!isApkInstalled(s, apps)) {
                            if (s.length() < 3) {
                                continue;
                            }
                            RemainedInfo info = new RemainedInfo();
                            info.packageName = s.substring(0, s.length() - 2);
                            info.path = "/data/app-lib/" + s;
                            info.dirSize = FileUtils.getDirSize(info.path);
                            try {
                                if (info.dirSize.equals("unknown") || Integer.parseInt(info.dirSize) > 4) {
                                    info.dirSize += "K";
                                    list.add(info);
                                }
                            } catch (Exception e) {

                            }
                        }
                    }
                }

            }
        }
        return list;

    }

    private static boolean isApkInstalled(String packageName, List<ApplicationInfo> apps) {
        boolean ret = false;
        if (packageName.startsWith("vmdl-")) {
            ret = true;
        } else {
            if (packageName.contains("-")) {
                packageName = packageName.substring(0, packageName.length() - 2);
            }
            for (ApplicationInfo s : apps) {

                if (s.packageName.equals(packageName)) {
                    ret = true;
                    break;
                }
            }
        }
        return ret;
    }
}
