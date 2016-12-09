package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/26/16.
 */
object FuckThemeManager {

    fun fuckThemeManager(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        val clsPageItem = XpUtils.findClass(loadPackageParam.classLoader, "com.android.thememanager.model.PageItem")
        if (clsPageItem != null) {
            XpUtils.findAndHookMethod("com.android.thememanager.controller.online.PageItemViewConverter", loadPackageParam.classLoader, "buildAdView", clsPageItem, XC_MethodReplacement.returnConstant(null))
        }

        val clsHybridView = XpUtils.findClass(loadPackageParam.classLoader, "miui.hybrid.HybridView")
        if (clsHybridView != null) {
            XpUtils.findAndHookMethod("com.android.thememanager.h5.ThemeHybridFragment\$BaseWebViewClient", loadPackageParam.classLoader, "shouldInterceptRequest", clsHybridView, String::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val url = param.args[1] as String
                    if (url.contains("AdCenter")) {
                        param.args[1] = "http://127.0.0.1/"
                    }

                }
            })
        }

    }
}
