package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

import java.util.HashMap

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
    }
}
