package com.rarnu.tools.neo.xposed.ads

import android.view.View
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.XposedHelpers
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 1/3/17.
 */
object FuckAccount {
    fun fuckAccount(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("miui.vip.VipPortraitView", loadPackageParam.classLoader, "showBottomDivider", java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                param.args[0] = false
            }
        })
        XpUtils.findAndHookMethod("miui.vip.VipPortraitView", loadPackageParam.classLoader, "setBanners", List::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                (param.args[0] as MutableList<*>?)?.clear()
            }

            @Throws(Throwable::class)
            override fun afterHookedMethod(param: MethodHookParam) {
                val v = XposedHelpers.getObjectField(param.thisObject, "mBannerGroup") as View?
                v?.visibility = View.GONE
            }
        })
    }

    fun fuckVip(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        val clsVisibleItems = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.vip.ui.tasklist.TaskItemsDataPreprocessor\$VisibleItems")
        if (clsVisibleItems != null) {
            XpUtils.findAndHookMethod("com.xiaomi.vip.ui.tasklist.TaskItemsDataPreprocessor", loadPackageParam.classLoader, "addBanners", clsVisibleItems, XC_MethodReplacement.returnConstant(null))
        }

    }
}