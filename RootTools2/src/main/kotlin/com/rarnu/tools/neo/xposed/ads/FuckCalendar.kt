package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.content.Intent
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage
import org.json.JSONObject

/**
 * Created by rarnu on 11/18/16.
 */
object FuckCalendar {

    fun fuckCalendar(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("com.miui.calendar.ad.AdUtils", loadPackageParam.classLoader, "canShowBrandAd", Context::class.java, XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.calendar.ad.AdService", loadPackageParam.classLoader, "onHandleIntent", Intent::class.java, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.calendar.card.single.local.SummarySingleCard", loadPackageParam.classLoader, "needShowAdBanner", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.calendar.card.single.custom.ad.AdSingleCard", loadPackageParam.classLoader, "needDisplay", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.calendar.ad.AdUtils", loadPackageParam.classLoader, "getAdConfigJson", Context::class.java, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.calendar.card.single.custom.RecommendSingleCard", loadPackageParam.classLoader, "needDisplay", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.calendar.card.single.custom.ad.LargeImageAdSingleCard", loadPackageParam.classLoader, "needDisplay", XC_MethodReplacement.returnConstant(false))
        val clsC = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.ad.internal.common.module.a\$c")
        if (clsC != null) {
            XpUtils.findAndHookMethod("com.xiaomi.ad.internal.common.module.a", loadPackageParam.classLoader, "b", clsC, XC_MethodReplacement.returnConstant(null))
        }
        XpUtils.findAndHookMethod("com.xiaomi.ad.common.pojo.Ad", loadPackageParam.classLoader, "parseJson", JSONObject::class.java, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.a.e", loadPackageParam.classLoader, "onAdInfo", String::class.java, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.calendar.util.DiskStringCache", loadPackageParam.classLoader, "getString", Context::class.java, String::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val key = param.args[1] as String
                if (key.startsWith("bottom_banner_is_closed_today")) {
                    param.result = "true"
                }
            }

            @Throws(Throwable::class)
            override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val key = param.args[1] as String
                if (key.startsWith("bottom_banner_is_closed_today")) {
                    param.result = "true"
                }
            }
        })
    }
}
