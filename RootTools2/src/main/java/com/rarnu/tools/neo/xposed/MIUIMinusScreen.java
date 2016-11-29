package com.rarnu.tools.neo.xposed;

import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.XSharedPreferences;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

/**
 * Created by rarnu on 11/17/16.
 */
public class MIUIMinusScreen implements IXposedHookLoadPackage {

    @Override
    public void handleLoadPackage(XC_LoadPackage.LoadPackageParam loadPackageParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (loadPackageParam.packageName.equals("com.miui.home")) {
            if (prefs.getBoolean(XpStatus.KEY_MINUS_SCREEN, false)) {
                XpUtils.findAndHookMethod("com.miui.home.launcher.X", loadPackageParam.classLoader, "cl", XC_MethodReplacement.returnConstant(true));
                XpUtils.findAndHookMethod("com.miui.home.launcher.DeviceConfig", loadPackageParam.classLoader, "needHideMinusScreen", XC_MethodReplacement.returnConstant(true));

            }
        }
    }
}

