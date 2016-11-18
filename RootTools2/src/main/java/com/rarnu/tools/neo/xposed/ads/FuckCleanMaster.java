package com.rarnu.tools.neo.xposed.ads;

import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_InitPackageResources;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

import java.util.Map;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckCleanMaster {

    public static void fuckCleanMaster(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        XpUtils.findAndHookMethod("com.miui.optimizecenter.result.DataModel", loadPackageParam.classLoader, "post", Map.class, XC_MethodReplacement.returnConstant(""));
        XpUtils.findAndHookMethod("com.miui.optimizecenter.config.MiStat", loadPackageParam.classLoader, "getChannel", XC_MethodReplacement.returnConstant("international"));
        Class<?> clsAdImageView = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.optimizecenter.widget.AdImageView");
        Class<?> clsAdvertisement = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.optimizecenter.result.Advertisement");
        if (clsAdImageView != null && clsAdvertisement != null) {
            XpUtils.findAndHookMethod("com.miui.optimizecenter.result.CleanResultActivity", loadPackageParam.classLoader, "startAdCountdown", clsAdImageView, clsAdvertisement, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.miui.optimizecenter.result.CleanResultActivity", loadPackageParam.classLoader, "addAdvertisementEvent", String.class, clsAdvertisement, XC_MethodReplacement.returnConstant(null));
        }
    }

    public static void fuckResource(XC_InitPackageResources.InitPackageResourcesParam initPackageResourcesParam) {
        initPackageResourcesParam.res.setReplacement(initPackageResourcesParam.packageName, "string", "no_network", "");
    }

}
