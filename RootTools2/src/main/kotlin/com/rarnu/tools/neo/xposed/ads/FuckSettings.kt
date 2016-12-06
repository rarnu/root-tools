package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.os.Bundle
import android.view.LayoutInflater
import android.view.ViewGroup
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XposedBridge
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 12/6/16.
 */
object FuckSettings {
    fun fuckSettings(loadPackageParam: XC_LoadPackage.LoadPackageParam) {

        XpUtils.findAndHookMethod("com.android.settings.device.MiuiDeviceDetailSettings", loadPackageParam.classLoader, "onInflateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                val mContext = param.result.javaClass.getDeclaredMethod("getContext")
                val ctx = mContext.invoke(param.result) as Context?
                val pkg = ctx?.packageManager?.getPackageInfo(ctx.packageName, 0)
                if (pkg != null) {
                    val verCode = pkg.versionCode
                    if (verCode == 23) {
                        val v = param.result as ViewGroup       // linearLayout
                        val s = v.getChildAt(0) as ViewGroup    // scrollView
                        val l = s.getChildAt(0) as ViewGroup    // linearLayout

                        (0..l.childCount - 1).map { l.getChildAt(it) }.forEach {
                            XposedBridge.log("onInflateView => ${it.javaClass.name}")
                            // TODO:
                        }
                    }
                }
            }
        })
    }
}