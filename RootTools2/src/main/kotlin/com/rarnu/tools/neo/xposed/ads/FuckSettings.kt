package com.rarnu.tools.neo.xposed.ads

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.FrameLayout
import android.widget.LinearLayout
import android.widget.ScrollView
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 12/6/16.
 */
object FuckSettings {
    fun fuckSettings(loadPackageParam: XC_LoadPackage.LoadPackageParam) {

        val clsDetailSettings = XpUtils.findClass(loadPackageParam.classLoader, "com.android.settings.device.MiuiDeviceDetailSettings")
        if (clsDetailSettings != null) {
            XpUtils.findAndHookMethod("com.android.settings.device.MiuiDeviceDetailSettings", loadPackageParam.classLoader, "onInflateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    val ll = param.result as LinearLayout
                    val sv = ll.getChildAt(0) as ScrollView
                    val li = sv.getChildAt(0) as LinearLayout
                    if (li.childCount == 5) {
                        val lay2 = li.getChildAt(1) as LinearLayout
                        lay2.getChildAt(0).visibility = View.GONE
                        lay2.getChildAt(1).visibility = View.GONE
                        lay2.getChildAt(2).visibility = View.GONE
                        lay2.getChildAt(7).visibility = View.GONE
                        lay2.getChildAt(8).visibility = View.GONE
                        lay2.getChildAt(9).visibility = View.GONE

                        (li.getChildAt(2) as FrameLayout).visibility = View.GONE
                        (li.getChildAt(3) as View).visibility = View.GONE
                        (li.getChildAt(4) as LinearLayout).visibility = View.GONE
                    }
                }
            })
        }

    }
}