package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.callbacks.XC_LoadPackage


/**
 * Created by rarnu on 11/18/16.
 */
object FuckCore {

    fun fuckCore(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("miui.os.SystemProperties", loadPackageParam.classLoader, "get", String::class.java, String::class.java, object : XC_MethodHook() {

            @Throws(Throwable::class)
            override fun afterHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                if (paramAnonymousMethodHookParam.args[0].toString() == "ro.product.mod_device") {
                    paramAnonymousMethodHookParam.result = "gemini_global"
                }
            }

            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                if (paramAnonymousMethodHookParam.args[0].toString() == "ro.product.mod_device") {
                    paramAnonymousMethodHookParam.result = "gemini_global"
                }
            }
        })
        XpUtils.setStaticBooleanField("miui.os.SystemProperties.Build", "IS_CM_CUSTOMIZATION_TEST", true)
        XpUtils.setStaticBooleanField("com.miui.internal.util", "IS_INTERNATIONAL_BUILD", true)
    }

}
