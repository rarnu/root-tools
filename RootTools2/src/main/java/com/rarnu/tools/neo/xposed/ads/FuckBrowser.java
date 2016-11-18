package com.rarnu.tools.neo.xposed.ads;

import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

import java.util.HashMap;
import java.util.List;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckBrowser {

    public static void fuckBrowser(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        XpUtils.findAndHookMethod("miui.browser.a.a", loadPackageParam.classLoader, "a", String.class, String.class, String.class, List.class, HashMap.class, XC_MethodReplacement.returnConstant(null));
        Class<?> clsA = XpUtils.findClass(loadPackageParam.classLoader, "com.a.a.d.a");
        if (clsA != null) {
            XpUtils.findAndHookMethod("com.android.browser.suggestion.SuggestItem$AdsInfo", loadPackageParam.classLoader, "deserialize", clsA, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.android.browser.homepage.HomepageBannerProvider$AdTrackingInfo", loadPackageParam.classLoader, "deserialize", clsA, XC_MethodReplacement.returnConstant(null));
        }
    }
}
