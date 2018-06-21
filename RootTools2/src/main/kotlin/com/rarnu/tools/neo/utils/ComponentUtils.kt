package com.rarnu.tools.neo.utils

import android.app.ActivityManager
import android.content.Context
import android.content.pm.PackageManager
import com.rarnu.kt.android.*

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

    fun getActivityList(ctx: Context, pkg: Package?): List<CompInfo> {
        val lstRet = arrayListOf<CompInfo>()
        val pm = ctx.packageManager
        val lst = pkg?.activities
        if (lst != null) {
            for (a in lst) {
                val info = CompInfo()
                info.component = a
                info.fullPackageName = a.componentName?.className
                info.enabled = pm.getComponentEnabledSetting(a.componentName) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                lstRet.add(info)
            }
        }
        lstRet.sortBy { it.compName }
        return lstRet
    }

    fun getServiceList(ctx: Context, pkg: Package?): List<CompInfo> {
        val lstRet = arrayListOf<CompInfo>()
        val pm = ctx.packageManager
        val lst = pkg?.services
        if (lst != null) {
            for (s in lst) {
                val info = CompInfo()
                info.component = s
                info.fullPackageName = s.componentName?.className
                info.enabled = pm.getComponentEnabledSetting(s.componentName) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                lstRet.add(info)
            }
        }
        lstRet.sortBy { it.compName }
        return lstRet
    }

    fun getReceiverList(ctx: Context, pkg: Package?): List<CompInfo> {
        val lstRet = arrayListOf<CompInfo>()
        val pm = ctx.packageManager
        val lst = pkg?.receivers
        if (lst != null) {
            for (r in lst) {
                val info = CompInfo()
                info.component = r
                info.fullPackageName = r.componentName?.className
                info.enabled = pm.getComponentEnabledSetting(r.componentName) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                lstRet.add(info)
            }
        }
        lstRet.sortBy { it.compName }
        return lstRet
    }

    fun getProviderList(ctx: Context, pkg: Package?): List<CompInfo> {
        val lstRet = arrayListOf<CompInfo>()
        val pm = ctx.packageManager
        val lst = pkg?.providers
        if (lst != null) {
            for (p in lst) {
                val info = CompInfo()
                info.component = p
                info.fullPackageName = p.componentName?.className
                info.enabled = pm.getComponentEnabledSetting(p.componentName) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                lstRet.add(info)
            }
        }
        lstRet.sortBy { it.compName }
        return lstRet
    }
}

class CompInfo {

    var component: Component? = null
    var enabled = false
    var position = 0
    var fullPackageName: String? = null

    val compName: String?
        get() = component?.className?.substringAfterLast("")

    val intents: List<String>
        get() {
            val result = arrayListOf<String>()
            if (component != null && component?.intents != null) {
                component?.intents?.filter { it.countActions() > 0 }?.forEach { a -> (0 until a.countActions()).mapTo(result) { a.getAction(it) } }
            }
            return result
        }

    fun appendIntents(str: String?): String? {
        var nstr = str
        val pa = component!!
        if (pa.intents != null) {
            if (pa.intents!!.isNotEmpty()) {
                for (aobj in pa.intents!!) {
                    if (aobj.countActions() > 0) {
                        for (i in 0 until aobj.countActions()) {
                            nstr += aobj.getAction(i).substringAfterLast("").replace("_", "").toLowerCase() + "<br />"
                        }
                    }
                }
            }
        }
        return nstr
    }

    fun isServiceRunning(context: Context): Boolean {
        var ret = false
        try {
            ret = ComponentUtils.isServiceRunning(context, component?.className)
        } catch (e: Throwable) {

        }
        return ret
    }

}