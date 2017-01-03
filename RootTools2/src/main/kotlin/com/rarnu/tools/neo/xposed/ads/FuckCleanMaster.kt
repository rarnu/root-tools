package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.content.Intent
import android.preference.PreferenceManager
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_InitPackageResources
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/18/16.
 */
object FuckCleanMaster {

    fun fuckCleanMaster(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("com.miui.optimizecenter.result.DataModel", loadPackageParam.classLoader, "post", Map::class.java, XC_MethodReplacement.returnConstant(""))
        XpUtils.findAndHookMethod("com.miui.optimizecenter.config.MiStat", loadPackageParam.classLoader, "getChannel", XC_MethodReplacement.returnConstant("international"))
        val clsAdImageView = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.optimizecenter.widget.AdImageView")
        val clsAdvertisement = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.optimizecenter.result.Advertisement")
        if (clsAdImageView != null && clsAdvertisement != null) {
            XpUtils.findAndHookMethod("com.miui.optimizecenter.result.CleanResultActivity", loadPackageParam.classLoader, "startAdCountdown", clsAdImageView, clsAdvertisement, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.optimizecenter.result.CleanResultActivity", loadPackageParam.classLoader, "addAdvertisementEvent", String::class.java, clsAdvertisement, XC_MethodReplacement.returnConstant(null))
        }
        XpUtils.findAndHookMethod("com.miui.optimizecenter.Application", loadPackageParam.classLoader, "attachBaseContext", Context::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun afterHookedMethod(param: MethodHookParam) {
                val pref = PreferenceManager.getDefaultSharedPreferences(param.thisObject as Context)
                pref.edit().putBoolean("key_information_setting_close", false).apply()
            }
        })
    }

    fun fuckSecurityCenter(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("com.miui.securitycenter.Application", loadPackageParam.classLoader, "attachBaseContext", Context::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun afterHookedMethod(param: MethodHookParam) {
                val pref = PreferenceManager.getDefaultSharedPreferences(param.thisObject as Context)
                pref.edit().putBoolean("key_information_setting_close", false).apply()
            }
        })
        XpUtils.findAndHookMethod("com.miui.securitycenter.service.i", loadPackageParam.classLoader, "oI", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.securitycenter.service.i", loadPackageParam.classLoader, "oy", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.securitycenter.service.i", loadPackageParam.classLoader, "a", String::class.java, Intent::class.java, XC_MethodReplacement.returnConstant(null))

    }

    fun fuckResource(initPackageResourcesParam: XC_InitPackageResources.InitPackageResourcesParam) {
        initPackageResourcesParam.res.setReplacement(initPackageResourcesParam.packageName, "string", "no_network", "")
    }

}
