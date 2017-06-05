package com.rarnu.tools.neo.utils

import android.content.Context
import android.content.Intent
import android.content.pm.ApplicationInfo
import android.content.pm.PackageInfo
import android.net.Uri
import android.os.Environment
import com.rarnu.tools.neo.data.AppInfo

object AppUtils {

    fun getSystemApps(ctx: Context): MutableList<AppInfo> {
        val pm = ctx.packageManager
        val pkgs = pm?.getInstalledPackages(0)
        val list = arrayListOf<AppInfo>()
        val listData = arrayListOf<AppInfo>()
        val listDisabled = arrayListOf<AppInfo>()
        if (pkgs != null) {
            pkgs.filter { it.packageName != ctx.packageName }.forEach {
                if (it.applicationInfo.flags and ApplicationInfo.FLAG_SYSTEM > 0) {
                    if (it.applicationInfo.enabled) {
                        list.add(AppInfo(
                                it.applicationInfo.loadLabel(pm).toString(),
                                try { it.applicationInfo.loadIcon(pm) } catch (e: Exception) { null },
                                it.packageName,
                                false,
                                it.versionName,
                                it.versionCode,
                                true,
                                true))
                    } else {
                        listDisabled.add(AppInfo(
                                it.applicationInfo.loadLabel(pm).toString(),
                                try { it.applicationInfo.loadIcon(pm) } catch (e: Exception) { null },
                                it.packageName,
                                true,
                                it.versionName,
                                it.versionCode,
                                true,
                                true))
                    }
                } else {
                    if (it.applicationInfo.enabled) {
                        listData.add(AppInfo(
                                it.applicationInfo.loadLabel(pm).toString(),
                                try { it.applicationInfo.loadIcon(pm) } catch (e: Exception) { null },
                                it.packageName,
                                false,
                                it.versionName,
                                it.versionCode,
                                false,
                                true))
                    } else {
                        listDisabled.add(AppInfo(
                                it.applicationInfo.loadLabel(pm).toString(),
                                try { it.applicationInfo.loadIcon(pm) } catch (e: Exception) { null },
                                it.packageName,
                                true,
                                it.versionName,
                                it.versionCode,
                                false,
                                true))
                    }
                }
            }
        }
        list.sortBy { it.name }
        if (listData.size > 0) {
            listData.sortBy { it.name }
            list.addAll(listData)
        }
        if (listDisabled.size > 0) {
            listDisabled.sortBy { it.name }
            list.addAll(listDisabled)
        }
        return list
    }

    fun getInstalledApps(ctx: Context): MutableList<AppInfo> {
        val pm = ctx.packageManager
        val pkgs = pm?.getInstalledPackages(0)
        val list = arrayListOf<AppInfo>()
        val listSystem = arrayListOf<AppInfo>()
        if (pkgs != null) {
            pkgs.filter { it.packageName != ctx.packageName && it.applicationInfo.enabled }.forEach {
                if (it.applicationInfo.flags and ApplicationInfo.FLAG_SYSTEM > 0) {
                    listSystem.add(AppInfo(
                            it.applicationInfo.loadLabel(pm).toString(),
                            try { it.applicationInfo.loadIcon(pm) } catch (e: Exception) { null },
                            it.packageName,
                            true,
                            it.versionName,
                            it.versionCode,
                            true,
                            false))
                } else {
                    list.add(AppInfo(
                            it.applicationInfo.loadLabel(pm).toString(),
                            try { it.applicationInfo.loadIcon(pm) } catch (e: Exception) { null },
                            it.packageName,
                            true,
                            it.versionName,
                            it.versionCode,
                            false,
                            false))
                }
            }
        }
        list.sortBy { it.name }
        if (listSystem.size > 0) {
            listSystem.sortBy { it.name }
            list.addAll(listSystem)
        }
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
