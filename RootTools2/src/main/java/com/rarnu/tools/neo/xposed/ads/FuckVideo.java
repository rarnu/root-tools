package com.rarnu.tools.neo.xposed.ads;

import android.content.Context;
import android.content.Loader;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_LoadPackage;
import org.json.JSONObject;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckVideo {

    public static void fuckVideo(XC_LoadPackage.LoadPackageParam loadPackageParam) {
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
        XpUtils.findAndHookMethod("com.video.ui.idata.iDataORM", loadPackageParam.classLoader, "getBooleanValue", Context.class, String.class, boolean.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                String key = (String) param.args[1];
                if (key.equals("show_title_ads") || key.equals("show_channel_title_ads")) {
                    param.setResult(false);
                }
            }

            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                String key = (String) param.args[1];
                if (key.equals("show_title_ads") || key.equals("show_channel_title_ads")) {
                    param.setResult(false);
                }
            }
        });
        XpUtils.findAndHookMethod("com.video.ui.idata.iDataORM", loadPackageParam.classLoader, "enabledAds", Context.class, XC_MethodReplacement.returnConstant(false));
        Class<?> clsAdListener = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.systemAdSolution.splashAd.IAdListener");
        if (clsAdListener != null) {
            XpUtils.findAndHookMethod("com.miui.ad.sdk.api.RemoteSystemSplashAdService", loadPackageParam.classLoader, "requestSystemSplashAd", clsAdListener, XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.miui.ad.sdk.api.SystemSplashAd", loadPackageParam.classLoader, "requestAd", Context.class, clsAdListener, XC_MethodReplacement.returnConstant(null));
            XpUtils.findAndHookMethod("com.miui.ad.sdk.api.SystemSplashAd", loadPackageParam.classLoader, "requestAd", clsAdListener, XC_MethodReplacement.returnConstant(null));
        }

        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "supportFrontAD", XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "supportPauseAD", XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "supportCornerAD", XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "skipAllAD", XC_MethodReplacement.returnConstant(true));
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "skipSDKAD", XC_MethodReplacement.returnConstant(true));
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "getMiAdFlag", XC_MethodReplacement.returnConstant(-1));

        XpUtils.findAndHookMethod("com.miui.videoplayer.ads.AdsContainer", loadPackageParam.classLoader, "setCornerAd", XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.miui.videoplayer.ads.AdsContainer", loadPackageParam.classLoader, "enableOfflineAds", XC_MethodReplacement.returnConstant(false));

        XpUtils.findAndHookMethod("com.miui.videoplayer.videoview.VideoViewContainer", loadPackageParam.classLoader, "playAd", XC_MethodReplacement.returnConstant(null));
        Class<?> clsVideoView = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.videoplayer.videoview.IVideoView");
        if (clsVideoView != null) {
            XpUtils.findAndHookMethod("com.miui.videoplayer.videoview.VideoViewContainer", loadPackageParam.classLoader, "playRealVideo", clsVideoView, boolean.class, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    param.args[1] = true;   // skipAd
                }
            });
        }
        XpUtils.findAndHookMethod("com.miui.videoplayer.videoview.VideoViewContainer", loadPackageParam.classLoader, "prepareRealVideoView", boolean.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                param.args[0] = true;  // haveAd
            }
        });

        XpUtils.findAndHookMethod("com.miui.videoplayer.ads.AdsService", loadPackageParam.classLoader, "doLaunch", String.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.miui.videoplayer.videoview.MiAdsVideoView", loadPackageParam.classLoader, "haveAd", XC_MethodReplacement.returnConstant(false));
        XpUtils.findAndHookMethod("com.xiaomi.miui.ad.listeners.impl.AdEventListenerImpl", loadPackageParam.classLoader, "onAdRequest", String.class, JSONObject.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.xiaomi.miui.ad.listeners.impl.AdEventListenerImpl", loadPackageParam.classLoader, "onAdClicked", String.class, JSONObject.class, XC_MethodReplacement.returnConstant(null));

        Class<?> clsBlock = XpUtils.findClass(loadPackageParam.classLoader, "com.tv.ui.metro.model.Block");
        Class<?> clsPool = XpUtils.findClass(loadPackageParam.classLoader, "android.support.v7.widget.RecyclerView$RecycledViewPool");
        if (clsBlock != null && clsPool != null) {
            XpUtils.findAndHookConstructor("com.video.ui.view.BlockAdapter", loadPackageParam.classLoader, Context.class, clsBlock, new XC_MethodHook() {
                @Override
                protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                    removeAds(param);
                }
            });
            XpUtils.findAndHookConstructor("com.video.ui.view.BlockAdapter", loadPackageParam.classLoader, Context.class, clsBlock, clsPool, new XC_MethodHook() {
                @Override
                protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                    removeAds(param);
                }
            });
            XpUtils.findAndHookMethod("com.video.ui.view.BlockAdapter", loadPackageParam.classLoader, "addGroup", clsBlock, new XC_MethodHook() {
                @Override
                protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                    removeAds(param);
                }
            });
            XpUtils.findAndHookMethod("com.video.ui.view.block.PortBlockView", loadPackageParam.classLoader, "initUI", clsBlock, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    Object block = param.args[0];
                    Class<?> clsBlock = block.getClass();
                    Field fBlocks = clsBlock.getDeclaredField("blocks");
                    fBlocks.setAccessible(true);
                    ArrayList blocks = (ArrayList) fBlocks.get(block);
                    for (int i = blocks.size() - 1; i >= 0; i--) {
                        Object o = blocks.get(i);
                        Field fUI = o.getClass().getSuperclass().getDeclaredField("ui_type");
                        Object oUI = fUI.get(o);
                        Method mId = oUI.getClass().getMethod("id");
                        int id = (int) mId.invoke(oUI);
                        if (id == 101 || id == 10001 || id == 282 || id == 257 || id == 221 ||
                                id == 501 || id == 502 || id == 503 || id == 601 | id == 602 || id == 603 || id == 604) {
                            blocks.remove(i);
                        }
                    }
                }
            });
        }

        if (clsBlock != null) {
            XpUtils.findAndHookMethod("com.video.ui.view.ListFragment", loadPackageParam.classLoader, "setBlockView", clsBlock, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    Object o = param.args[0];
                    Field fBlocks = o.getClass().getDeclaredField("blocks");
                    fBlocks.setAccessible(true);
                    fBlocks.set(o, null);
                    Field fFooters = o.getClass().getDeclaredField("footers");
                    fFooters.setAccessible(true);
                    fFooters.set(o, null);
                }
            });
        }

        Class<?> clsVideo = XpUtils.findClass(loadPackageParam.classLoader, "com.tv.ui.metro.model.VideoItem");
        if (clsVideo != null) {
            XpUtils.findAndHookMethod("com.video.ui.view.DetailFragment", loadPackageParam.classLoader, "updateVideo", clsVideo, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    Object oVideo = param.args[0];
                    Field fBlocks = oVideo.getClass().getDeclaredField("blocks");
                    fBlocks.setAccessible(true);
                    fBlocks.set(oVideo, null);
                    Field fHeaders = oVideo.getClass().getDeclaredField("headers");
                    fHeaders.setAccessible(true);
                    fHeaders.set(oVideo, null);
                }
            });
        }

        XpUtils.findAndHookMethod("com.video.ui.view.DetailFragment", loadPackageParam.classLoader, "onCreateView", LayoutInflater.class, ViewGroup.class, Bundle.class, new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                Object fragment = param.thisObject;
                Field fR1 = fragment.getClass().getDeclaredField("relative_region");
                Field fR2 = fragment.getClass().getDeclaredField("headers_region");
                FrameLayout r1 = (FrameLayout) fR1.get(fragment);
                FrameLayout r2 = (FrameLayout) fR2.get(fragment);
                r1.setVisibility(View.GONE);
                r2.setVisibility(View.GONE);
            }
        });
        XpUtils.findAndHookMethod("com.video.ui.view.DetailFragment", loadPackageParam.classLoader, "checkAdsPresentVisibility", View.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.findAndHookMethod("com.video.ui.view.ListFragment", loadPackageParam.classLoader, "checkAdsPresentVisibility", View.class, XC_MethodReplacement.returnConstant(null));

        Class<?> clsGenericBlock = XpUtils.findClass(loadPackageParam.classLoader, "com.tv.ui.metro.model.GenericBlock");
        if (clsGenericBlock != null) {
            XpUtils.findAndHookMethod("com.video.ui.view.user.MyVideoFragment", loadPackageParam.classLoader, "onLoadFinished", Loader.class, clsGenericBlock, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    param.args[1] = null;
                }
            });
        }
    }

    private static void removeAds(XC_MethodHook.MethodHookParam param) {
        try {
            Class<?> clsThis = param.thisObject.getClass();
            Field fBlockRootArrayList = clsThis.getDeclaredField("mBlockRootArrayList");
            fBlockRootArrayList.setAccessible(true);
            ArrayList mBlockRootArrayList = (ArrayList) fBlockRootArrayList.get(param.thisObject);
            for (int i = mBlockRootArrayList.size() - 1; i >= 0; i--) {
                Object b = mBlockRootArrayList.get(i);
                Field fUI = b.getClass().getSuperclass().getDeclaredField("ui_type");
                fUI.setAccessible(true);
                Object oUI = fUI.get(b);
                Method mId = oUI.getClass().getMethod("id");
                int id = (int) mId.invoke(oUI);
                if (id == 101 || id == 10001 || id == 282 || id == 257 || id == 221 ||
                        id == 501 || id == 502 || id == 503 || id == 601 | id == 602 || id == 603 || id == 604) {
                    mBlockRootArrayList.remove(i);
                }
            }
        } catch (Exception e) {
        }
    }
}
