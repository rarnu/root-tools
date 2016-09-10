package com.rarnu.tools.neo.utils;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import com.rarnu.tools.neo.data.AppInfo;

import java.util.ArrayList;
import java.util.List;

public class AppUtils {

    public static List<AppInfo> getSystemApps(Context ctx) {
        PackageManager pm = ctx.getPackageManager();
        List<PackageInfo> pkgs = pm.getInstalledPackages(0);

        List<AppInfo> list = new ArrayList<>();
        List<AppInfo> listDisabled = new ArrayList<>();
        if (pkgs != null) {
            for (PackageInfo pkg : pkgs) {
                if ((pkg.applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) > 0) {
                    if (pkg.applicationInfo.enabled) {
                        list.add(new AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                false,
                                pkg.versionName,
                                pkg.versionCode,
                                true,
                                true
                        ));
                    } else {
                        listDisabled.add(new AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                true,
                                pkg.versionName,
                                pkg.versionCode,
                                true,
                                true
                        ));
                    }
                }
            }
        }
        list.addAll(listDisabled);
        return list;
    }

    public static List<AppInfo> getInstalledApps(Context ctx) {
        PackageManager pm = ctx.getPackageManager();
        List<PackageInfo> pkgs = pm.getInstalledPackages(0);
        List<AppInfo> list = new ArrayList<>();
        List<AppInfo> listSystem = new ArrayList<>();
        if (pkgs != null) {
            for (PackageInfo pkg : pkgs) {
                if (pkg.applicationInfo.enabled) {
                    if ((pkg.applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) > 0) {
                        listSystem.add(new AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                true,
                                pkg.versionName,
                                pkg.versionCode,
                                true,
                                false
                        ));
                    } else {
                        list.add(new AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                true,
                                pkg.versionName,
                                pkg.versionCode,
                                false,
                                false
                        ));
                    }

                }
            }
        }
        list.addAll(listSystem);
        return list;
    }

    public static boolean isMIUI(Context ctx) {
        PackageManager pm = ctx.getPackageManager();
        String[] pkgs = new String[] {"com.miui.core", "com.miui.system"};
        boolean isMIUI = true;
        PackageInfo pi;
        for (String s : pkgs) {
            try {
                pi = pm.getPackageInfo(s, 0);
                isMIUI = pi != null;
            } catch (Exception e) {
                isMIUI = false;
            }
            if (!isMIUI) {
                break;
            }
        }
        return isMIUI;
    }

}
