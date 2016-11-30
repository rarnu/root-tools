package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/18/16.
 */
object FuckWeather {

    fun fuckWeather(loadPackageParam: XC_LoadPackage.LoadPackageParam) {

        XpUtils.findAndHookMethod("com.miui.weather2.tools.ToolUtils", loadPackageParam.classLoader, "checkCommericalStatue", Context::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                paramAnonymousMethodHookParam.result = false
            }
        })

        XpUtils.findAndHookMethod("com.miui.weather2.tools.ToolUtils", loadPackageParam.classLoader, "canRequestCommercialInfo", Context::class.java, XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.weather2.tools.ToolUtils", loadPackageParam.classLoader, "checkCommericalStatue", Context::class.java, XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.weather2.ActivityDailyForecastDetail", loadPackageParam.classLoader, "ep", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.weather2.ActivityDailyForecastDetail", loadPackageParam.classLoader, "eq", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.weather2.ActivityDailyForecastDetail", loadPackageParam.classLoader, "er", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.weather2.ActivityDailyForecastDetail", loadPackageParam.classLoader, "eo", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.weather2.structures.DailyForecastAdData", loadPackageParam.classLoader, "isAdInfosExistence", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.weather2.structures.DailyForecastAdData", loadPackageParam.classLoader, "isAdTitleExistence", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.weather2.structures.DailyForecastAdData", loadPackageParam.classLoader, "isLandingPageUrlExistence", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.weather2.structures.DailyForecastAdData", loadPackageParam.classLoader, "isUseSystemBrowserExistence", XC_MethodReplacement.returnConstant(false))
    }
}
