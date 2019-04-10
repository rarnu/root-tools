@file:Suppress("Duplicates", "unused")

package com.rarnu.tools.neo.utils

import android.app.ActivityManager
import android.content.ComponentName
import android.content.Context
import android.content.pm.PackageInfo
import android.content.pm.PackageManager
import android.util.Log

object ComponentUtils {

    fun isServiceRunning(context: Context, className: String?): Boolean {
        val activityManager = context.getSystemService(Context.ACTIVITY_SERVICE) as ActivityManager
        @Suppress("DEPRECATION")
        val serviceList = activityManager.getRunningServices(30)
        if (serviceList.isEmpty()) {
            return false
        }
        return serviceList.any { it.service.className == className }
    }

    fun getActivityList(ctx: Context, pkg: PackageInfo): List<CompInfo> {
        val lstRet = mutableListOf<CompInfo>()
        val pm = ctx.packageManager
        val lst = pkg.activities
        if (lst != null) {
            for (a in lst) {
                val info = CompInfo()
                Log.e("RootTools", "Activity => $a")
                info.pureName = a.name.substringAfterLast(".")
                info.componentClassName = a.name
                info.componentPackageName = a.packageName
                info.enabled = pm.getComponentEnabledSetting(ComponentName(a.packageName, a.name)) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                lstRet.add(info)
            }
        }
        lstRet.sortBy { it.pureName }
        return lstRet
    }

    fun getServiceList(ctx: Context, pkg: PackageInfo): List<CompInfo> {
        val lstRet = mutableListOf<CompInfo>()
        val pm = ctx.packageManager
        val lst = pkg.services
        if (lst != null) {
            for (s in lst) {
                val info = CompInfo()
                info.pureName = s.name.substringAfterLast(".")
                info.componentClassName = s.name
                info.componentPackageName = s.packageName
                info.enabled = pm.getComponentEnabledSetting(ComponentName(s.packageName, s.name)) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                lstRet.add(info)
            }
        }
        lstRet.sortBy { it.pureName }
        return lstRet
    }

    fun getReceiverList(ctx: Context, pkg: PackageInfo): List<CompInfo> {
        val lstRet = mutableListOf<CompInfo>()
        val pm = ctx.packageManager
        val lst = pkg.receivers
        if (lst != null) {
            for (r in lst) {
                val info = CompInfo()
                info.pureName = r.name.substringAfterLast(".")
                info.componentClassName = r.name
                info.componentPackageName = r.packageName
                info.enabled = pm.getComponentEnabledSetting(ComponentName(r.packageName, r.name)) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                lstRet.add(info)
            }
        }
        lstRet.sortBy { it.pureName }
        return lstRet
    }

    fun getProviderList(ctx: Context, pkg: PackageInfo): List<CompInfo> {
        val lstRet = mutableListOf<CompInfo>()
        val pm = ctx.packageManager
        val lst = pkg.providers
        if (lst != null) {
            for (p in lst) {
                val info = CompInfo()
                info.pureName = p.name.substringAfterLast(".")
                info.componentClassName = p.name
                info.componentPackageName = p.packageName
                info.enabled = pm.getComponentEnabledSetting(ComponentName(p.packageName, p.name)) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                lstRet.add(info)
            }
        }
        lstRet.sortBy { it.pureName }
        return lstRet
    }
}

class CompInfo {

    var enabled = false
    var position = 0
    var pureName = ""
    var componentClassName: String? = null
    var componentPackageName: String? = null

    fun isServiceRunning(context: Context): Boolean {
        var ret = false
        try {
            ret = ComponentUtils.isServiceRunning(context, componentClassName)
        } catch (e: Throwable) {

        }
        return ret
    }

}