package com.rarnu.tools.neo.xposed.ads;

import android.content.Context;
import android.content.Intent;
import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_LoadPackage;
import org.json.JSONObject;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckCalendar {

    public static void fuckCalendar(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        XpUtils.findAndHookMethod("com.miui.calendar.ad.AdUtils", loadPackageParam.classLoader, "canShowBrandAd", Context.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.miui.calendar.ad.AdService", loadPackageParam.classLoader, "onHandleIntent", Intent.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.miui.calendar.card.single.local.SummarySingleCard", loadPackageParam.classLoader, "needShowAdBanner", XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.miui.calendar.card.single.custom.ad.AdSingleCard", loadPackageParam.classLoader, "needDisplay", XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.miui.calendar.ad.AdUtils", loadPackageParam.classLoader, "getAdConfigJson", Context.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.miui.calendar.card.single.custom.RecommendSingleCard", loadPackageParam.classLoader, "needDisplay", XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.miui.calendar.card.single.custom.ad.LargeImageAdSingleCard", loadPackageParam.classLoader, "needDisplay", XC_MethodReplacement.returnConstant(false));
        Class<?> clsC = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.ad.internal.common.module.a$c");
        if (clsC != null) {
            XpUtils.findAndHookMethod("com.xiaomi.ad.internal.common.module.a", loadPackageParam.classLoader, "b", clsC, XC_MethodReplacement.returnConstant(null));
        }
        XpUtils.findAndHookMethod("com.xiaomi.ad.common.pojo.Ad", loadPackageParam.classLoader, "parseJson", JSONObject.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.a.e", loadPackageParam.classLoader, "onAdInfo", String.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.miui.calendar.util.DiskStringCache", loadPackageParam.classLoader, "getString", Context.class, String.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                String key = (String) param.args[1];

                if (key.startsWith("bottom_banner_is_closed_today")) {
                    param.setResult("true");
                }
            }
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                String key = (String) param.args[1];
                if (key.startsWith("bottom_banner_is_closed_today")) {
                    param.setResult("true");
                }
            }
        });
    }
}
