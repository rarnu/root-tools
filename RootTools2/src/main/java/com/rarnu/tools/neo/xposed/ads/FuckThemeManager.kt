package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
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

    }
}
