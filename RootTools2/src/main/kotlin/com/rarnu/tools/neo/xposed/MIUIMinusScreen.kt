package com.rarnu.tools.neo.xposed

import com.rarnu.xfunc.*
import de.robv.android.xposed.IXposedHookLoadPackage
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.XSharedPreferences
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/17/16.
 */
class MIUIMinusScreen : XposedPackage() {
    override fun hook(pkg: XposedPkg) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (pkg.packageName == "com.miui.home") {
            if (prefs.getBoolean(XpStatus.KEY_MINUS_SCREEN, false)) {
                pkg.findClass("com.miui.home.launcher.X").findMethod("cl").hook { replace { result = true } }
                pkg.findClass("com.miui.home.launcher.DeviceConfig").findMethod("needHideMinusScreen").hook { replace { result = true } }
            }
        }
    }

}

