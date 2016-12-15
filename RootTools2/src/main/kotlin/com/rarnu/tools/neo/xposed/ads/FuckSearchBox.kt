package com.rarnu.tools.neo.xposed.ads

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
        val clsQueryView = XpUtils.findClass(loadPackageParam.classLoader, "com.android.quicksearchbox.ui.HotQueryView")
        if (clsQueryView != null) {
            XpUtils.findAndHookMethod("com.android.quicksearchbox.util.HotWordsUtil", loadPackageParam.classLoader, "setHotQueryView", clsQueryView, XC_MethodReplacement.returnConstant(null))
        }
    }
}
