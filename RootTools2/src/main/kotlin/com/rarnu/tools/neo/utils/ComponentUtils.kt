@file:Suppress("Duplicates", "unused")

package com.rarnu.tools.neo.utils

import android.app.ActivityManager
import android.content.ComponentName
import android.content.Context
import android.content.pm.PackageInfo
import android.content.pm.PackageManager
import android.util.Log

object ComponentUtils {

    @Suppress("DEPRECATION")
    fun isServiceRunning(context: Context, className: String?) = (context.getSystemService(Context.ACTIVITY_SERVICE) as ActivityManager).getRunningServices(30).any { it.service.className == className }

    fun getActivityList(ctx: Context, pkg: PackageInfo) = mutableListOf<CompInfo>().apply {
        val pm = ctx.packageManager
        pkg.activities?.forEach {
            add(CompInfo().apply {
                Log.e("RootTools", "Activity => $it")
                pureName = it.name.substringAfterLast(".")
                componentClassName = it.name
                componentPackageName = it.packageName
                enabled = pm.getComponentEnabledSetting(ComponentName(it.packageName, it.name)) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
            })
        }
        sortBy { it.pureName }
    }.toList()

    fun getServiceList(ctx: Context, pkg: PackageInfo) = mutableListOf<CompInfo>().apply {
        val pm = ctx.packageManager
        pkg.services?.forEach {
            add(CompInfo().apply {
                pureName = it.name.substringAfterLast(".")
                componentClassName = it.name
                componentPackageName = it.packageName
                enabled = pm.getComponentEnabledSetting(ComponentName(it.packageName, it.name)) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
            })
        }
        sortBy { it.pureName }
    }.toList()

    fun getReceiverList(ctx: Context, pkg: PackageInfo) = mutableListOf<CompInfo>().apply {
        val pm = ctx.packageManager
        pkg.receivers?.forEach {
            add(CompInfo().apply {
                pureName = it.name.substringAfterLast(".")
                componentClassName = it.name
                componentPackageName = it.packageName
                enabled = pm.getComponentEnabledSetting(ComponentName(it.packageName, it.name)) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
            })
        }
        sortBy { it.pureName }
    }.toList()

    fun getProviderList(ctx: Context, pkg: PackageInfo) = mutableListOf<CompInfo>().apply {
        val pm = ctx.packageManager
        pkg.providers?.forEach {
            add(CompInfo().apply {
                pureName = it.name.substringAfterLast(".")
                componentClassName = it.name
                componentPackageName = it.packageName
                enabled = pm.getComponentEnabledSetting(ComponentName(it.packageName, it.name)) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
            })
        }
        sortBy { it.pureName }
    }.toList()
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