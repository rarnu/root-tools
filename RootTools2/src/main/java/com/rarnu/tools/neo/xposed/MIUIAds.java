package com.rarnu.tools.neo.xposed;


import android.app.Activity;
import android.app.Application;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import com.rarnu.tools.neo.utils.ComponentUtils;
import de.robv.android.xposed.*;
import de.robv.android.xposed.callbacks.XC_InitPackageResources;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

import javax.net.ssl.SSLPeerUnverifiedException;
import java.lang.annotation.Target;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.*;

public class MIUIAds implements IXposedHookLoadPackage, IXposedHookInitPackageResources {

    private static XC_LoadPackage.LoadPackageParam loadPackageParam;

    public void handleLoadPackage(XC_LoadPackage.LoadPackageParam paramLoadPackageParam) throws Throwable {
        loadPackageParam = paramLoadPackageParam;
        patchcode();
    }

    private void patchcode() {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (!prefs.getBoolean(XpStatus.KEY_REMOVEAD, false)) {
            return;
        }

        if (loadPackageParam.packageName.equals("com.miui.core")) {
            XpUtils.findAndHookMethod("miui.os.SystemProperties", loadPackageParam.classLoader, "get", String.class, String.class, new XC_MethodHook() {

                protected void afterHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    if (paramAnonymousMethodHookParam.args[0].toString().equals("ro.product.mod_device")) {
                        paramAnonymousMethodHookParam.setResult("gemini_global");
                    }
                }

                protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    if (paramAnonymousMethodHookParam.args[0].toString().equals("ro.product.mod_device")) {
                        paramAnonymousMethodHookParam.setResult("gemini_global");
                    }
                }
            });
            return;
        }

        if (loadPackageParam.packageName.equals("com.miui.cleanmaster")) {
            XpUtils.findAndHookMethod("com.miui.optimizecenter.result.DataModel", loadPackageParam.classLoader, "post", Map.class, XC_MethodReplacement.returnConstant(""));
            XpUtils.findAndHookMethod("com.miui.optimizecenter.config.MiStat", loadPackageParam.classLoader, "getChannel", XC_MethodReplacement.returnConstant("international"));
            Class<?> clsAdImageView = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.optimizecenter.widget.AdImageView");
            Class<?> clsAdvertisement = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.optimizecenter.result.Advertisement");
            if (clsAdImageView != null && clsAdvertisement != null) {
                XpUtils.findAndHookMethod("com.miui.optimizecenter.result.CleanResultActivity", loadPackageParam.classLoader, "startAdCountdown", clsAdImageView, clsAdvertisement, XC_MethodReplacement.returnConstant(null));
                XpUtils.findAndHookMethod("com.miui.optimizecenter.result.CleanResultActivity", loadPackageParam.classLoader, "addAdvertisementEvent", String.class, clsAdvertisement, XC_MethodReplacement.returnConstant(null));
            }
            return;
        }

        if (loadPackageParam.packageName.equals("com.miui.video")) {
            XpUtils.findAndHookMethod("com.miui.videoplayer.ads.DynamicAd", loadPackageParam.classLoader, "replaceList", List.class, String.class, new XC_MethodHook() {
                protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam)
                        throws Throwable {
                    paramAnonymousMethodHookParam.args[0] = null;
                    paramAnonymousMethodHookParam.args[1] = null;
                }
            });
            XpUtils.findAndHookMethod("com.video.ui.view.AdView", loadPackageParam.classLoader, "getAdsBlock", Context.class, XC_MethodReplacement.returnConstant(null));
            Class<?> clsCallback = XpUtils.findClass(loadPackageParam.classLoader, "com.video.ui.idata.SyncServiceHelper$Callback");
            if (clsCallback != null) {
                XpUtils.findAndHookMethod("com.video.ui.idata.SyncServiceHelper", loadPackageParam.classLoader, "fetchAds", Context.class, clsCallback, XC_MethodReplacement.returnConstant(null));
            }
            XpUtils.findAndHookMethod("com.video.ui.idata.iDataORM", loadPackageParam.classLoader, "getBooleanValue", Context.class, String.class, boolean.class, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    String key = (String) param.args[1];
                    if (key.equals("debug_mode") || key.equals("show_first_ads") || key.equals("ads_show_homekey") || key.equals("startup_ads_loop") || key.equals("app_upgrade_splash")) {
                        param.setResult(false);
                    }
                }

                @Override
                protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                    String key = (String) param.args[1];
                    if (key.equals("debug_mode") || key.equals("show_first_ads") || key.equals("ads_show_homekey") || key.equals("startup_ads_loop") || key.equals("app_upgrade_splash")) {
                        param.setResult(false);
                    }
                }
            });
            XpUtils.findAndHookMethod("com.video.ui.idata.iDataORM", loadPackageParam.classLoader, "getStringValue", Context.class, String.class, String.class, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    String key = (String) param.args[1];
                    if (key.equals("startup_ads")) {
                        param.setResult(null);
                    }
                }

                @Override
                protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                    String key = (String) param.args[1];
                    if (key.equals("startup_ads")) {
                        param.setResult(null);
                    }
                }
            });
            return;
        }

        if (loadPackageParam.packageName.equals("com.android.fileexplorer")) {
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "isAdEnable", Context.class, String.class, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "supportAd", XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "ifAdShowByCloudForNetwork", Context.class, String.class, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "getHomePageHotVideoTipSwitch", Context.class, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "getHomePageHotVideoTopicUri", Context.class, XC_MethodReplacement.returnConstant(""));
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "getAdStyleName", Context.class, String.class, XC_MethodReplacement.returnConstant(""));
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "tryInit", Context.class, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "isVideoEnable", Context.class, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "isStickerEnable", Context.class, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.android.fileexplorer.util.XLUtil", loadPackageParam.classLoader, "isNetworkAvailable", Context.class, XC_MethodReplacement.returnConstant(false));

            XpUtils.findAndHookMethod("com.xunlei.adlibrary.XunleiADSdk", loadPackageParam.classLoader, "setup", Context.class, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.xunlei.adlibrary.analytics.xunlei.AdStatHelper", loadPackageParam.classLoader, "init", Context.class, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.android.fileexplorer.video.upload.VideoItemManager", loadPackageParam.classLoader, "initLoad", XC_MethodReplacement.returnConstant(null));
            return;
        }

        if (loadPackageParam.packageName.equals("com.miui.player")) {

            Class<?> clsListener = XpUtils.findClass(loadPackageParam.classLoader, "com.android.volley.Response$Listener");
            Class<?> clsErrorListener = XpUtils.findClass(loadPackageParam.classLoader, "com.android.volley.Response$ErrorListener");
            Class<?> clsAdInfo = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.util.AdUtils$AdInfo");

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
            return;
        }

        if (loadPackageParam.packageName.equals("com.android.providers.downloads.ui")) {
            XpUtils.findAndHookMethod("com.android.providers.downloads.ui.recommend.config.ADConfig", loadPackageParam.classLoader, "OSSupportAD", new XC_MethodHook() {
                protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    paramAnonymousMethodHookParam.setResult(false);
                }
            });
            XpUtils.findAndHookMethod("com.android.providers.downloads.ui.utils.BuildUtils", loadPackageParam.classLoader, "isCmTestBuilder", XC_MethodReplacement.returnConstant(true));
            return;
        }

        if (loadPackageParam.packageName.equals("com.miui.weather2")) {
            XpUtils.findAndHookMethod("com.miui.weather2.tools.ToolUtils", loadPackageParam.classLoader, "checkCommericalStatue", Context.class, new XC_MethodHook() {
                protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    paramAnonymousMethodHookParam.setResult(false);
                }
            });
            XpUtils.findAndHookMethod("com.miui.weather2.tools.ToolUtils", loadPackageParam.classLoader, "canRequestCommercialInfo", Context.class, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.miui.weather2.tools.ToolUtils", loadPackageParam.classLoader, "checkCommericalStatue", Context.class, XC_MethodReplacement.returnConstant(false));
            return;
        }

        if (loadPackageParam.packageName.equals("com.android.quicksearchbox")) {
            XpUtils.findAndHookMethod("com.android.quicksearchbox.ui.LocalListView", loadPackageParam.classLoader, "updateHotQuery", List.class, int.class, new XC_MethodReplacement() {
                @Override
                protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                    return null;
                }
            });
            XpUtils.findAndHookMethod("com.android.quicksearchbox.util.HotWordsUtil", loadPackageParam.classLoader, "setHotQueryView", "com.android.quicksearchbox.ui.HotQueryView", new XC_MethodHook() {
                protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    paramAnonymousMethodHookParam.setResult(null);
                }
            });
            return;
        }

        if (loadPackageParam.packageName.equals("com.android.mms")) {
            XpUtils.findAndHookMethod("com.android.mms.ui.MessageUtils", loadPackageParam.classLoader, "isMessagingTemplateAllowed", Context.class, new XC_MethodHook() {
                protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    Context mc = (Context) paramAnonymousMethodHookParam.args[0];
                    paramAnonymousMethodHookParam.setResult(!mc.getClass().getName().toLowerCase().contains("app"));
                }
            });
            XpUtils.findAndHookMethod("com.android.mms.ui.SingleRecipientConversationActivity", loadPackageParam.classLoader, "showMenuMode", new XC_MethodReplacement() {
                @Override
                protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                    return null;
                }
            });
            XpUtils.findAndHookMethod("com.android.mms.util.MiStatSdkHelper", loadPackageParam.classLoader, "recordBottomMenuShown", String.class, new XC_MethodReplacement() {
                @Override
                protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                    return null;
                }
            });
            return;
        }

        if (loadPackageParam.packageName.equals("com.android.calendar")) {
            XpUtils.findAndHookMethod("com.miui.calendar.ad.AdUtils", loadPackageParam.classLoader, "canShowBrandAd", Context.class, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.miui.calendar.ad.AdService", loadPackageParam.classLoader, "onHandleIntent", Intent.class, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.miui.calendar.card.single.local.SummarySingleCard", loadPackageParam.classLoader, "needShowAdBanner", XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.miui.calendar.card.single.custom.ad.AdSingleCard", loadPackageParam.classLoader, "needDisplay", XC_MethodReplacement.returnConstant(false));
            return;
        }

        if (loadPackageParam.packageName.equals("com.miui.systemAdSolution")) {
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
            return;
        }

        if (loadPackageParam.packageName.equals("com.android.browser")) {
            XpUtils.findAndHookMethod("miui.browser.a.a", loadPackageParam.classLoader, "a", String.class, String.class, String.class, List.class, HashMap.class, XC_MethodReplacement.returnConstant(null));
            Class<?> clsA = XpUtils.findClass(loadPackageParam.classLoader, "com.a.a.d.a");
            if (clsA != null) {
                XpUtils.findAndHookMethod("com.android.browser.suggestion.SuggestItem$AdsInfo", loadPackageParam.classLoader, "deserialize", clsA, XC_MethodReplacement.returnConstant(null));
                XpUtils.findAndHookMethod("com.android.browser.homepage.HomepageBannerProvider$AdTrackingInfo", loadPackageParam.classLoader, "deserialize", clsA, XC_MethodReplacement.returnConstant(null));
            }
        }
    }

    @Override
    public void handleInitPackageResources(XC_InitPackageResources.InitPackageResourcesParam initPackageResourcesParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (!prefs.getBoolean(XpStatus.KEY_REMOVEAD, false)) {
            return;
        }
        if (initPackageResourcesParam.packageName.equals("com.miui.cleanmaster")) {
            initPackageResourcesParam.res.setReplacement(initPackageResourcesParam.packageName, "string", "no_network", "");
        }
    }
}
