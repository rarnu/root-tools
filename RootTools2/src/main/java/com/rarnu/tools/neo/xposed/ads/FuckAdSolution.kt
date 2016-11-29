package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.os.Bundle
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/18/16.
 */
object FuckAdSolution {

    fun fuckAdSolution(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.common.b.c", loadPackageParam.classLoader, "a", String::class.java, Context::class.java, XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.common.b.c", loadPackageParam.classLoader, "j", Context::class.java, String::class.java, XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.common.b.d", loadPackageParam.classLoader, "m", Context::class.java, String::class.java, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.systemAdSolution.landingPage.h", loadPackageParam.classLoader, "a", String::class.java, String::class.java, Integer.TYPE, XC_MethodReplacement.returnConstant(null))
        val clsB = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.ad.internal.pojo.b")
        if (clsB != null) {
            XpUtils.findAndHookMethod("com.xiaomi.ad.internal.server.h", loadPackageParam.classLoader, "a", clsB, Bundle::class.java, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.xiaomi.ad.internal.server.h", loadPackageParam.classLoader, "b", clsB, Bundle::class.java, XC_MethodReplacement.returnConstant(null))
        }
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.ui.SplashAdView", loadPackageParam.classLoader, "onAction", String::class.java, XC_MethodReplacement.returnConstant(null))
    }
}
