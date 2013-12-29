package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.RemainedInfo;

import java.util.ArrayList;
import java.util.List;

public class RemainedFilesUtils {
    public static List<RemainedInfo> getRemainedFiles(Context context) {
        // get remained files list
        List<RemainedInfo> list = new ArrayList<RemainedInfo>();

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

        List<ApplicationInfo> apps = GlobalInstance.pm.getInstalledApplications(0);

        if (apps != null) {

            for (String s : arrData) {
                if (!isApkInstalled(s, apps)) {
                    RemainedInfo info = new RemainedInfo();
                    info.packageName = s;
                    info.path = "/data/data/" + s;
                    list.add(info);
                }
            }
            for (String s : arrLib) {
                if (!isApkInstalled(s, apps)) {
                    RemainedInfo info = new RemainedInfo();
                    info.packageName = s.substring(0, s.length() - 2);
                    info.path = "/data/app-lib/" + s;
                    list.add(info);
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
