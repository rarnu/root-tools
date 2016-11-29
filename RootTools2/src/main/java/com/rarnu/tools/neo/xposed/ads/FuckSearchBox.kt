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
        XpUtils.findAndHookMethod("com.android.quicksearchbox.ui.LocalListView", loadPackageParam.classLoader, "updateHotQuery", List::class.java, Integer.TYPE, object : XC_MethodReplacement() {
            @Throws(Throwable::class)
            override fun replaceHookedMethod(param: XC_MethodHook.MethodHookParam): Any? {
                return null
            }
        })
        XpUtils.findAndHookMethod("com.android.quicksearchbox.util.HotWordsUtil", loadPackageParam.classLoader, "setHotQueryView", "com.android.quicksearchbox.ui.HotQueryView", object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam?) {
                paramAnonymousMethodHookParam!!.result = null
            }
        })
    }
}
