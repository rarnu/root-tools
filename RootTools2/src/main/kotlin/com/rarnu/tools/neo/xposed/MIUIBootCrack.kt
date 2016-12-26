package com.rarnu.tools.neo.xposed

import android.content.pm.PackageManager
import android.content.pm.Signature
import de.robv.android.xposed.IXposedHookLoadPackage
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.XSharedPreferences
import de.robv.android.xposed.XposedBridge
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 12/26/16.
 */
class MIUIBootCrack : IXposedHookLoadPackage {

    override fun handleLoadPackage(param: XC_LoadPackage.LoadPackageParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (prefs.getBoolean(XpStatus.KEY_PREVENT_FREEZE_REVERSE, false)) {
            if (param.packageName == "android" || param.packageName == "com.miui.system" || param.packageName == "miui.system") {
                val clsSMS = XpUtils.findClass(param.classLoader, "com.miui.server.SecurityManagerService")
                XposedBridge.log("MIUIBootCrack load => ${param.packageName}, cls: ${clsSMS != null}")
                if (clsSMS != null) {
                    XposedBridge.log("MIUIBootCrack inject service.odex")
                    XpUtils.findAndHookMethod("com.miui.server.SecurityManagerService", param.classLoader, "checkSysAppCrack", XC_MethodReplacement.returnConstant(false))
                    XpUtils.findAndHookMethod("com.miui.server.SecurityManagerService", param.classLoader, "checkEnabled", PackageManager::class.java, String::class.java, XC_MethodReplacement.returnConstant(null))
                    XpUtils.findAndHookMethod("com.miui.server.SecurityManagerService", param.classLoader, "enforcePlatformSignature", Array<Signature>::class.java, XC_MethodReplacement.returnConstant(null))
                }
            }
        }
    }
}