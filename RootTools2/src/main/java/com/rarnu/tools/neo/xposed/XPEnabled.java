package com.rarnu.tools.neo.xposed;

import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

public class XPEnabled implements IXposedHookLoadPackage {
    @Override
    public void handleLoadPackage(XC_LoadPackage.LoadPackageParam loadPackageParam) throws Throwable {
        if (loadPackageParam.packageName.equals(XpStatus.PKGNAME)) {
            XposedBridge.log("RootTools Activated.");
            XpUtils.findAndHookMethod("com.rarnu.tools.neo.xposed.XpStatus", loadPackageParam.classLoader, "isEnable", XC_MethodReplacement.returnConstant(true));
        }
    }
}
