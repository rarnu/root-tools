package com.rarnu.tools.neo.xposed.ads;

import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

/**
 * Created by rarnu on 11/26/16.
 */
public class FuckThemeManager {

    public static void fuckThemeManager(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        Class<?> clsPageItem = XpUtils.findClass(loadPackageParam.classLoader, "com.android.thememanager.model.PageItem");
        if (clsPageItem != null) {
            XpUtils.findAndHookMethod("com.android.thememanager.controller.online.PageItemViewConverter", loadPackageParam.classLoader, "buildAdView", clsPageItem, XC_MethodReplacement.returnConstant(null));
        }

    }
}
