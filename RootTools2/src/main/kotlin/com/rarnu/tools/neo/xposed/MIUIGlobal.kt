package com.rarnu.tools.neo.xposed

import de.robv.android.xposed.*
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 9/17/16.
 */
class MIUIGlobal : IXposedHookLoadPackage {
    @Throws(Throwable::class)
    override fun handleLoadPackage(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        if (loadPackageParam.packageName == "com.miui.analytics") {
            XpUtils.findAndHookMethod("com.miui.analytics.internal.l", loadPackageParam.classLoader, "k", XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.analytics.internal.l", loadPackageParam.classLoader, "f", XC_MethodReplacement.returnConstant(""))
            XpUtils.findAndHookMethod("com.miui.analytics.internal.l", loadPackageParam.classLoader, "h", XC_MethodReplacement.returnConstant(null))

            XpUtils.findAndHookMethod("com.miui.analytics.internal.l$1", loadPackageParam.classLoader, "run", XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.analytics.internal.l$2", loadPackageParam.classLoader, "run", XC_MethodReplacement.returnConstant(null))
        }

        if (loadPackageParam.packageName == "android" || loadPackageParam.processName == "android" || loadPackageParam.packageName == "com.miui.system" || loadPackageParam.packageName == "miui.system") {
            XpUtils.findAndHookMethod("android.os.SystemProperties", loadPackageParam.classLoader, "getInt", String::class.java, Integer.TYPE, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    if (param.args[0] == "ro.debuggable") {
                        param.result = 1
                    }
                }
            })
            XpUtils.findAndHookMethod("android.os.SystemProperties", loadPackageParam.classLoader, "getBoolean", String::class.java, java.lang.Boolean.TYPE, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    if (param.args[0] == "persist.sys.miui_optimization") {
                        param.result = true
                    }
                }
            })

            XpUtils.findAndHookMethod("android.os.SystemProperties", loadPackageParam.classLoader, "get", String::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    if (param.args[0] == "ro.miui.cts" || param.args[0] == "ro.miui.cta") {
                        param.result = "1"
                    }
                }
            })

            XpUtils.findAndHookMethod("miui.os.SystemProperties", loadPackageParam.classLoader, "getInt", String::class.java, Integer.TYPE, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    if (param.args[0] == "ro.debuggable") {
                        param.result = 1
                    }
                }
            })

            XpUtils.findAndHookMethod("miui.os.SystemProperties", loadPackageParam.classLoader, "getBoolean", String::class.java, java.lang.Boolean.TYPE, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    if (param.args[0] == "persist.sys.miui_optimization") {
                        param.result = true
                    }
                }
            })

            XpUtils.findAndHookMethod("miui.os.SystemProperties", loadPackageParam.classLoader, "get", String::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    if (param.args[0] == "ro.miui.cts" || param.args[0] == "ro.miui.cta") {
                        param.result = "1"
                    }
                }
            })

        }
    }
}
