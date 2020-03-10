package com.rarnu.tools.neo.xposed

import com.rarnu.xfunc.*
import de.robv.android.xposed.XSharedPreferences

/**
 * Created by rarnu on 1/24/17.
 */
class MIUIUpdate : XposedPackage() {
    override fun hook(pkg: XposedPkg) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (pkg.packageName == "com.android.updater") {
            if (prefs.getBoolean(XpStatus.KEY_NOUPDATE, false)) {
                pkg.findClass("com.android.updater.CurrentLogActivity").findMethod("getDefaultUrl").hook { replace { result = "" } }
                pkg.findClass("com.android.updater.b.k").findMethod("bW").hook { replace { result = "" } }
                pkg.findClass("com.android.updater.b.c").findMethod("a", String::class.java, List::class.java).hook {
                    before {
                        args[0] = ""
                    }
                }
                val clsInfo = pkg.findClass("com.android.updater.UpdateInfo")
                if (clsInfo != null) {
                    pkg.findClass("com.android.updater.az").findMethod("a", Integer.TYPE, clsInfo, java.lang.Boolean.TYPE, Integer.TYPE, Integer.TYPE).hook {
                        before {
                            args[1] = null
                        }
                    }
                }
            }
        }
    }

}