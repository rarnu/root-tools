package com.rarnu.tools.neo.xposed;

import de.robv.android.xposed.IXposedHookInitPackageResources;
import de.robv.android.xposed.XSharedPreferences;
import de.robv.android.xposed.callbacks.XC_InitPackageResources;


public class MIUISearchBar implements IXposedHookInitPackageResources {

    public void handleInitPackageResources(XC_InitPackageResources.InitPackageResourcesParam paramInitPackageResourcesParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (prefs.getBoolean(XpStatus.KEY_REMOVESEARCHBAR, false)) {
            if (paramInitPackageResourcesParam.packageName.equals("com.android.systemui")) {
                paramInitPackageResourcesParam.res.setReplacement(paramInitPackageResourcesParam.packageName, "bool", "config_show_statusbar_search", false);
            }
        }
    }
}
