package com.rarnu.tools.neo.xposed.ads;

import android.app.Activity;
import android.content.Context;
import android.net.Uri;
import android.view.View;
import android.view.ViewGroup;
import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckMusic {

    public static void fuckMusic(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        Class<?> clsListener = XpUtils.findClass(loadPackageParam.classLoader, "com.android.volley.Response$Listener");
        Class<?> clsErrorListener = XpUtils.findClass(loadPackageParam.classLoader, "com.android.volley.Response$ErrorListener");
        final Class<?> clsAdInfo = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.util.AdUtils$AdInfo");

        XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "isAdEnable", XC_MethodReplacement.returnConstant(false));
        if (clsListener != null && clsErrorListener != null) {
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "getPlayAd", clsListener, clsErrorListener, XC_MethodReplacement.returnConstant(null));
        }
        XpUtils.findAndHookMethod("com.miui.player.util.ExperimentsHelper", loadPackageParam.classLoader, "isAdEnabled", XC_MethodReplacement.returnConstant(false));
        if (clsAdInfo != null) {
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "handleDeepLinkUrl", Activity.class, clsAdInfo, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "showAlertAndDownload", Activity.class, clsAdInfo, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "handleAdClick", Activity.class, clsAdInfo, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "postPlayAdStat", String.class, clsAdInfo, XC_MethodReplacement.returnConstant(null));
        }
        XpUtils.findAndHookMethod("com.miui.player.phone.view.NowplayingAlbumPage", loadPackageParam.classLoader, "getPlayAd", XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.miui.player.util.Configuration", loadPackageParam.classLoader, "isCmTest", XC_MethodReplacement.returnConstant(true));
        XpUtils.findAndHookMethod("com.miui.player.util.Configuration", loadPackageParam.classLoader, "isCpLogoVisiable", XC_MethodReplacement.returnConstant(false));

        // fuck the ad under account
        Class<?> clsAdInfo2 = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.music.online.model.AdInfo");
        if (clsAdInfo != null) {
            XpUtils.findAndHookMethod("com.miui.player.util.AdForbidManager", loadPackageParam.classLoader, "recordAdInfo", clsAdInfo2, int.class, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.miui.player.util.AdForbidManager", loadPackageParam.classLoader, "addForbidInfo", String.class, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.miui.player.util.AdForbidManager", loadPackageParam.classLoader, "isForbidden", String.class, XC_MethodReplacement.returnConstant(true));
        }
        XpUtils.findAndHookMethod("com.miui.player.hybrid.feature.GetAdInfo", loadPackageParam.classLoader, "addAdQueryParams", Context.class, Uri.class, XC_MethodReplacement.returnConstant(""));

        XpUtils.findAndHookMethod("com.miui.player.display.view.cell.BannerAdItemCell", loadPackageParam.classLoader, "onFinishInflate", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                Class<?> clsThis = param.thisObject.getClass();
                View vThis = (View) param.thisObject;
                Field fText = clsThis.getDeclaredField("mClose");
                fText.setAccessible(true);
                View vText = (View) fText.get(param.thisObject);
                vText.setVisibility(View.GONE);
                Field fImg = clsThis.getDeclaredField("mImage");
                fImg.setAccessible(true);
                View vImg  = (View) fImg.get(param.thisObject);
                vImg.setVisibility(View.GONE);
                ViewGroup.LayoutParams lp = vThis.getLayoutParams();
                lp.height = 0;
                vThis.setLayoutParams(lp);
            }
        });
//        Class<?> clsDisplayItem = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.display.model.DisplayItem");
//        if (clsDisplayItem != null) {
//            XpUtils.findAndHookMethod("com.miui.player.display.view.cell.BannerAdItemCell", loadPackageParam.classLoader, "onBindItem", clsDisplayItem, int.class, new XC_MethodHook() {
//                @Override
//                protected void afterHookedMethod(MethodHookParam param) throws Throwable {
//                    XposedBridge.log("BannerAdItemCell onBindItem");
//                    Method fRemove = param.thisObject.getClass().getSuperclass().getDeclaredMethod("remove");
//                    fRemove.setAccessible(true);
//                    fRemove.invoke(param.thisObject);
//                }
//            });
//        }
    }

}
