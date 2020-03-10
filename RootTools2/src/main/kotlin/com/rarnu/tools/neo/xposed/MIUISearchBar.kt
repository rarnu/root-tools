package com.rarnu.tools.neo.xposed

import com.rarnu.xfunc.XposedRes
import com.rarnu.xfunc.XposedResource
import de.robv.android.xposed.XSharedPreferences

class MIUISearchBar : XposedResource() {
    override fun hook(res: XposedRes) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (res.packageName == "com.android.systemui") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVESEARCHBAR, false)) {
                try {
                    res.res.setReplacement(res.packageName, "bool", "config_show_statusbar_search", false)
                } catch (t: Throwable) {

                }
            }
        }
    }
}
