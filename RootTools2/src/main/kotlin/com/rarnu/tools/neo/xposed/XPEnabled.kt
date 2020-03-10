package com.rarnu.tools.neo.xposed

import com.rarnu.xfunc.*
import de.robv.android.xposed.IXposedHookLoadPackage
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.XposedBridge
import de.robv.android.xposed.callbacks.XC_LoadPackage

class XPEnabled: XposedPackage() {
    override fun hook(pkg: XposedPkg) {
        if (pkg.packageName == XpStatus.PKGNAME) {
            XposedBridge.log("RootTools Activated.")
            pkg.findClass("com.rarnu.tools.neo.xposed.XpStatus").findMethod("isEnable").hook { after { result = true } }
        }
    }

}
