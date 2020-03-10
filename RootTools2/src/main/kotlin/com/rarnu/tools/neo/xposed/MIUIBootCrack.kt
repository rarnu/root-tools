package com.rarnu.tools.neo.xposed

import android.content.pm.PackageManager
import android.content.pm.Signature
import com.rarnu.xfunc.*
import de.robv.android.xposed.*

class MIUIBootCrack : XposedPackage() {
    override fun hook(pkg: XposedPkg) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (prefs.getBoolean(XpStatus.KEY_PREVENT_FREEZE_REVERSE, false)) {
            if (pkg.packageName == "android" || pkg.packageName == "com.miui.system" || pkg.packageName == "miui.system") {
                pkg.findClass("com.miui.server.SecurityManagerService")?.apply {
                    findMethod("checkSysAppCrack").hook { replace { result = false } }
                    findMethod("checkEnabled", PackageManager::class.java, String::class.java).hook { replace { result = null } }
                    findMethod("enforcePlatformSignature", Array<Signature>::class.java).hook { replace { result = null } }
                }
            }
        }
    }

}
