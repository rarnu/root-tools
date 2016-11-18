package com.rarnu.tools.neo.xposed.ads;

import android.content.Context;
import android.os.Bundle;
import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckAdSolution {

    public static void fuckAdSolution(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.common.b.c", loadPackageParam.classLoader, "a", String.class, Context.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.common.b.c", loadPackageParam.classLoader, "j", Context.class, String.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.common.b.d", loadPackageParam.classLoader, "m", Context.class, String.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.miui.systemAdSolution.landingPage.h", loadPackageParam.classLoader, "a", String.class, String.class, int.class, XC_MethodReplacement.returnConstant(null));
        Class<?> clsB = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.ad.internal.pojo.b");
        if (clsB != null) {
            XpUtils.findAndHookMethod("com.xiaomi.ad.internal.server.h", loadPackageParam.classLoader, "a", clsB, Bundle.class, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.xiaomi.ad.internal.server.h", loadPackageParam.classLoader, "b", clsB, Bundle.class, XC_MethodReplacement.returnConstant(null));
        }
        XpUtils.findAndHookMethod("com.xiaomi.ad.internal.ui.SplashAdView", loadPackageParam.classLoader, "onAction", String.class, XC_MethodReplacement.returnConstant(null));
    }
}
