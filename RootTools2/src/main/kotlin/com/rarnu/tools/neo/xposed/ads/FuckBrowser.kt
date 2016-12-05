package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage
import java.util.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckBrowser {

    fun fuckBrowser(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("miui.browser.a.a", loadPackageParam.classLoader, "a", String::class.java, String::class.java, String::class.java, List::class.java, HashMap::class.java, XC_MethodReplacement.returnConstant(null))
        val clsA = XpUtils.findClass(loadPackageParam.classLoader, "com.a.a.d.a")
        if (clsA != null) {
            XpUtils.findAndHookMethod("com.android.browser.suggestion.SuggestItem\$AdsInfo", loadPackageParam.classLoader, "deserialize", clsA, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.android.browser.homepage.HomepageBannerProvider\$AdTrackingInfo", loadPackageParam.classLoader, "deserialize", clsA, XC_MethodReplacement.returnConstant(null))
        }

        // 8.2.15
        XpUtils.findAndHookMethod("com.android.browser.jx", loadPackageParam.classLoader, "ak", XC_MethodReplacement.returnConstant(true))
        XpUtils.findAndHookMethod("com.android.browser.suggestion.al", loadPackageParam.classLoader, "e", XC_MethodReplacement.returnConstant(0))
        XpUtils.findAndHookMethod("com.android.browser.view.at", loadPackageParam.classLoader, "setItemCount", Integer.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                var i = param.args[0] as Int
                if (i == 2) {
                    i = 1
                }
                param.args[0] = i
            }
        })


        // 8.4.4
        XpUtils.findAndHookMethod("com.android.browser.homepage.bk", loadPackageParam.classLoader, "e", XC_MethodReplacement.returnConstant(0))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "ak", XC_MethodReplacement.returnConstant(true))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "K", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "k", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "am", XC_MethodReplacement.returnConstant(1))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "an", XC_MethodReplacement.returnConstant(1))

        XpUtils.findAndHookMethod("com.android.browser.suggestion.al", loadPackageParam.classLoader, "f", XC_MethodReplacement.returnConstant(0))
        val clsSuggestItem = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.suggestion.SuggestItem")
        if (clsSuggestItem != null) {
            XpUtils.findAndHookMethod("com.android.browser.suggestion.ap", loadPackageParam.classLoader, "a", clsSuggestItem, XC_MethodReplacement.returnConstant(null))
        }

        XpUtils.findAndHookMethod("miui.browser.a", loadPackageParam.classLoader, "a", String::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                val str = param.args[0] as String?
                if (str == "v6_apk_download_guide_to_market") {
                    param.result = false
                }
            }
            @Throws(Throwable::class)
            override fun afterHookedMethod(param: MethodHookParam) {
                val str = param.args[0] as String?
                if (str == "v6_apk_download_guide_to_market") {
                    param.result = false
                }
            }
        })
        XpUtils.findAndHookMethod("com.android.browser.or", loadPackageParam.classLoader, "a", String::class.java, String::class.java, String::class.java, XC_MethodReplacement.returnConstant(null))

        // 8.5.3
        XpUtils.findAndHookMethod("com.android.browser.view.au", loadPackageParam.classLoader, "setItemCount", Integer.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                var i = param.args[0] as Int
                if (i == 2) {
                   i = 1
                }
                param.args[0] = i
            }
        })
        XpUtils.findAndHookMethod("com.android.browser.os", loadPackageParam.classLoader, "a", String::class.java, String::class.java, String::class.java, XC_MethodReplacement.returnConstant(null))

        val clsBrowserActivity = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.BrowserActivity")
        val clsTu = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.tu")
        val clsOs = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.os")
        if (clsBrowserActivity != null && clsTu != null && clsOs != null) {
            XpUtils.findAndHookConstructor("com.android.browser.kc", loadPackageParam.classLoader, clsBrowserActivity, clsTu, clsOs, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val o = param.thisObject
                    val fC = o.javaClass.getDeclaredField("c")
                    fC.isAccessible = true
                    fC.set(null, true)
                }
            })
        }

    }
}
