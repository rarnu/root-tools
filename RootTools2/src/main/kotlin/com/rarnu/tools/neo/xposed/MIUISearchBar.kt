package com.rarnu.tools.neo.xposed

import de.robv.android.xposed.IXposedHookInitPackageResources
import de.robv.android.xposed.XSharedPreferences
import de.robv.android.xposed.callbacks.XC_InitPackageResources


class MIUISearchBar : IXposedHookInitPackageResources {

    @Throws(Throwable::class)
    override fun handleInitPackageResources(paramInitPackageResourcesParam: XC_InitPackageResources.InitPackageResourcesParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (paramInitPackageResourcesParam.packageName == "com.android.systemui") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVESEARCHBAR, false)) {
                try {
                    paramInitPackageResourcesParam.res.setReplacement(paramInitPackageResourcesParam.packageName, "bool", "config_show_statusbar_search", false)
                } catch (t: Throwable) {

                }
            }
        }
    }
}
