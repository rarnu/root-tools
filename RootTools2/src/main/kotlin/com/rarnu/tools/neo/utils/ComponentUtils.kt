package com.rarnu.tools.neo.utils

import android.content.Context
import android.content.pm.PackageManager
import com.rarnu.tools.neo.data.CompInfo

@SuppressWarnings("Duplicates")
object ComponentUtils {

    fun getActivityList(ctx: Context?, pkg: Any?): MutableList<CompInfo> {
        val lstComponentInfo = arrayListOf<CompInfo>()
        if (ctx != null) {
            val pm = ctx.packageManager
            val lst = PackageParserUtils.packageActivities(pkg)
            if (lst != null) {
                for (a in lst) {
                    val aa = PackageParserUtils.Activity.fromComponent(a)
                    val info = CompInfo()
                    info.component = aa
                    info.fullPackageName = aa?.getComponentName()?.className
                    info.enabled = pm.getComponentEnabledSetting(aa?.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                    lstComponentInfo.add(info)
                }
            }
        }
        lstComponentInfo.sortBy { it.compName }
        return lstComponentInfo
    }

    fun getServiceList(ctx: Context?, pkg: Any?): MutableList<CompInfo> {
        val lstComponentInfo = arrayListOf<CompInfo>()
        if (ctx != null) {
            val pm = ctx.packageManager
            val lst = PackageParserUtils.packageServices(pkg)
            if (lst != null) {
                for (a in lst) {
                    val aa = PackageParserUtils.Service.fromComponent(a)
                    val info = CompInfo()
                    info.component = aa
                    info.fullPackageName = aa?.getComponentName()?.className
                    info.enabled = pm.getComponentEnabledSetting(aa?.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                    lstComponentInfo.add(info)
                }
            }
        }
        lstComponentInfo.sortBy { it.compName }
        return lstComponentInfo
    }

    fun getReceiverList(ctx: Context?, pkg: Any?): MutableList<CompInfo> {
        val lstComponentInfo = arrayListOf<CompInfo>()
        if (ctx != null) {
            val pm = ctx.packageManager
            val lst = PackageParserUtils.packageReceivers(pkg)
            if (lst != null) {
                for (a in lst) {
                    val aa = PackageParserUtils.Activity.fromComponent(a)
                    val info = CompInfo()
                    info.component = aa
                    info.fullPackageName = aa?.getComponentName()?.className
                    info.enabled = pm.getComponentEnabledSetting(aa?.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                    lstComponentInfo.add(info)
                }
            }
        }
        lstComponentInfo.sortBy { it.compName }
        return lstComponentInfo
    }

    fun getProviderList(ctx: Context?, pkg: Any?): MutableList<CompInfo> {
        val lstComponentInfo = arrayListOf<CompInfo>()
        if (ctx != null) {
            val pm = ctx.packageManager
            val lst = PackageParserUtils.packageProviders(pkg)
            if (lst != null) {
                for (a in lst) {
                    val aa = PackageParserUtils.Provider.fromComponent(a)
                    val info = CompInfo()
                    info.component = aa
                    info.fullPackageName = aa?.getComponentName()?.className
                    info.enabled = pm.getComponentEnabledSetting(aa?.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                    lstComponentInfo.add(info)
                }
            }
        }
        lstComponentInfo.sortBy { it.compName }
        return lstComponentInfo
    }

}
