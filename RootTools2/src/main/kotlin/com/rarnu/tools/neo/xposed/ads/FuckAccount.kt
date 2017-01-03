package com.rarnu.tools.neo.xposed.ads

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
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

        // VipPortraitView
        XpUtils.findAndHookMethod("com.xiaomi.account.ui.n", loadPackageParam.classLoader, "onCreateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun afterHookedMethod(param: MethodHookParam) {
                val ojT = XposedHelpers.getObjectField(param.thisObject, "jT")
                val oBannerGroup = XposedHelpers.getObjectField(ojT, "mBannerGroup") as View?
                oBannerGroup?.visibility = View.GONE
            }
        })

        XpUtils.findAndHookMethod("com.xiaomi.account.ui.o", loadPackageParam.classLoader, "onCreateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun afterHookedMethod(param: MethodHookParam) {
                val oJU = XposedHelpers.getObjectField(param.thisObject, "jU")
                val oBannerGroup = XposedHelpers.getObjectField(oJU, "mBannerGroup") as View?
                oBannerGroup?.visibility = View.GONE
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