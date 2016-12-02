package com.rarnu.tools.neo.utils

import android.content.Context
import android.content.Intent
import android.content.pm.ApplicationInfo
import android.content.pm.PackageInfo
import android.net.Uri
import android.os.Environment
import com.rarnu.tools.neo.data.AppInfo

object AppUtils {

    fun getSystemApps(ctx: Context?): MutableList<AppInfo> {
        val pm = ctx?.packageManager
        val pkgs = pm?.getInstalledPackages(0)

        val list = arrayListOf<AppInfo>()
        val listData = arrayListOf<AppInfo>()
        val listDisabled = arrayListOf<AppInfo>()
        if (pkgs != null) {
            for (pkg in pkgs) {
                if (pkg.applicationInfo.flags and ApplicationInfo.FLAG_SYSTEM > 0) {
                    if (pkg.applicationInfo.enabled) {
                        list.add(AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                false,
                                pkg.versionName,
                                pkg.versionCode,
                                true,
                                true))
                    } else {
                        listDisabled.add(AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                true,
                                pkg.versionName,
                                pkg.versionCode,
                                true,
                                true))
                    }
                } else {
                    if (pkg.applicationInfo.enabled) {
                        listData.add(AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                false,
                                pkg.versionName,
                                pkg.versionCode,
                                false,
                                true))
                    } else {
                        listDisabled.add(AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                true,
                                pkg.versionName,
                                pkg.versionCode,
                                false,
                                true))
                    }
                }
            }
        }
        list.addAll(listData)
        list.addAll(listDisabled)
        return list
    }

    fun getInstalledApps(ctx: Context?): MutableList<AppInfo> {
        val pm = ctx?.packageManager
        val pkgs = pm?.getInstalledPackages(0)
        val list = arrayListOf<AppInfo>()
        val listSystem = arrayListOf<AppInfo>()
        if (pkgs != null) {
            for (pkg in pkgs) {
                if (pkg.applicationInfo.enabled) {
                    if (pkg.applicationInfo.flags and ApplicationInfo.FLAG_SYSTEM > 0) {
                        listSystem.add(AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                true,
                                pkg.versionName,
                                pkg.versionCode,
                                true,
                                false))
                    } else {
                        list.add(AppInfo(
                                pkg.applicationInfo.loadLabel(pm).toString(),
                                pkg.applicationInfo.loadIcon(pm),
                                pkg.packageName,
                                true,
                                pkg.versionName,
                                pkg.versionCode,
                                false,
                                false))
                    }

                }
            }
        }
        list.addAll(listSystem)
        return list
    }

    fun isMIUI(ctx: Context?): Boolean {
        val pm = ctx?.packageManager
        val pkgs = arrayOf("com.miui.core", "com.miui.system")
        var isMIUI = true
        var pi: PackageInfo?
        for (s in pkgs) {
            try {
                pi = pm?.getPackageInfo(s, 0)
                isMIUI = pi != null
            } catch (e: Exception) {
                isMIUI = false
            }

            if (!isMIUI) {
                break
            }
        }
        return isMIUI
    }

    fun doScanMedia(context: Context?) =
            context?.sendBroadcast(Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, Uri.parse("file://" + Environment.getExternalStorageDirectory().absolutePath)))

}
