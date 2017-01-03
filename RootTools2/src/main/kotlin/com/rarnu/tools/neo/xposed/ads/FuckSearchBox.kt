package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/18/16.
 */
object FuckSearchBox {

    fun fuckSearchBox(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("com.android.quicksearchbox.ui.LocalListView", loadPackageParam.classLoader, "updateHotQuery", List::class.java, Integer.TYPE, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.android.quicksearchbox.ui.LocalListView", loadPackageParam.classLoader, "updateHotQuery", List::class.java, Integer.TYPE, java.lang.Boolean.TYPE, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.android.quicksearchbox.util.Util", loadPackageParam.classLoader, "isShowHotView", Context::class.java, XC_MethodReplacement.returnConstant(false))
        val clsQueryView = XpUtils.findClass(loadPackageParam.classLoader, "com.android.quicksearchbox.ui.HotQueryView")
        if (clsQueryView != null) {
            XpUtils.findAndHookMethod("com.android.quicksearchbox.util.HotWordsUtil", loadPackageParam.classLoader, "setHotQueryView", clsQueryView, XC_MethodReplacement.returnConstant(null))
        }
        XpUtils.findAndHookMethod("com.android.quicksearchbox.ui.LocalListView", loadPackageParam.classLoader, "updateBanners", XC_MethodReplacement.returnConstant(null))

        XpUtils.findAndHookMethod("com.android.quicksearchbox.SearchSettingsImpl", loadPackageParam.classLoader, "getShowHot", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.android.quicksearchbox.xiaomi.HotWordsProvider", loadPackageParam.classLoader, "isHotWordBackgroundRandom", Context::class.java, XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.android.quicksearchbox.xiaomi.HotWordsProvider", loadPackageParam.classLoader, "setHotWordBackgroundRandom", java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                param.args[0] = false
            }
        })
    }
}
