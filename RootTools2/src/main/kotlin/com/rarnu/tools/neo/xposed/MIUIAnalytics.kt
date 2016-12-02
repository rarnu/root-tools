package com.rarnu.tools.neo.xposed

import de.robv.android.xposed.IXposedHookLoadPackage
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 9/17/16.
 */
class MIUIAnalytics : IXposedHookLoadPackage {
    @Throws(Throwable::class)
    override fun handleLoadPackage(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        if (loadPackageParam.packageName == "com.miui.analytics") {
            XpUtils.findAndHookMethod("com.miui.analytics.internal.l", loadPackageParam.classLoader, "k", XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.analytics.internal.l", loadPackageParam.classLoader, "f", XC_MethodReplacement.returnConstant(""))
            XpUtils.findAndHookMethod("com.miui.analytics.internal.l", loadPackageParam.classLoader, "h", XC_MethodReplacement.returnConstant(null))

            XpUtils.findAndHookMethod("com.miui.analytics.internal.l$1", loadPackageParam.classLoader, "run", XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.analytics.internal.l$2", loadPackageParam.classLoader, "run", XC_MethodReplacement.returnConstant(null))
        }
    }
}
