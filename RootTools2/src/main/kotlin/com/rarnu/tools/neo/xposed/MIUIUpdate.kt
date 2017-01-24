package com.rarnu.tools.neo.xposed

import de.robv.android.xposed.IXposedHookLoadPackage
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.XSharedPreferences
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 1/24/17.
 */
class MIUIUpdate : IXposedHookLoadPackage {

    @Throws(Throwable::class)
    override fun handleLoadPackage(paramLoadPackageParam: XC_LoadPackage.LoadPackageParam) {

        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (paramLoadPackageParam.packageName == "com.android.updater") {
            if (prefs.getBoolean(XpStatus.KEY_NOUPDATE, false)) {
                XpUtils.findAndHookMethod("com.android.updater.CurrentLogActivity", paramLoadPackageParam.classLoader, "getDefaultUrl", XC_MethodReplacement.returnConstant(""))
                XpUtils.findAndHookMethod("com.android.updater.b.k", paramLoadPackageParam.classLoader, "bW", XC_MethodReplacement.returnConstant(""))
                XpUtils.findAndHookMethod("com.android.updater.b.c", paramLoadPackageParam.classLoader, "a", String::class.java, List::class.java, object : XC_MethodHook() {
                    @Throws(Throwable::class)
                    override fun beforeHookedMethod(param: MethodHookParam) {
                        param.args[0] = ""
                    }
                })
                val clsInfo = XpUtils.findClass(paramLoadPackageParam.classLoader, "com.android.updater.UpdateInfo")
                if (clsInfo != null) {
                    XpUtils.findAndHookMethod("com.android.updater.az", paramLoadPackageParam.classLoader, "a", Integer.TYPE, clsInfo, java.lang.Boolean.TYPE, Integer.TYPE, Integer.TYPE, object : XC_MethodHook() {
                        @Throws(Throwable::class)
                        override fun beforeHookedMethod(param: MethodHookParam) {
                            param.args[1] = null
                        }
                    })
                }
            }
        }
    }
}