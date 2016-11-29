package com.rarnu.tools.neo.xposed

import de.robv.android.xposed.IXposedHookLoadPackage
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.XposedBridge
import de.robv.android.xposed.callbacks.XC_LoadPackage

class XPEnabled : IXposedHookLoadPackage {
    @Throws(Throwable::class)
    override fun handleLoadPackage(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        if (loadPackageParam.packageName == XpStatus.PKGNAME) {
            XposedBridge.log("RootTools Activated.")
            XpUtils.findAndHookMethod("com.rarnu.tools.neo.xposed.XpStatus", loadPackageParam.classLoader, "isEnable", XC_MethodReplacement.returnConstant(true))
        }
    }
}
