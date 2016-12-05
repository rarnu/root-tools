package com.rarnu.tools.neo.xposed.ads

import android.app.Activity
import android.content.Context
import android.net.Uri
import android.view.View
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/18/16.
 */
object FuckMusic {

    fun fuckMusic(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        val clsListener = XpUtils.findClass(loadPackageParam.classLoader, "com.android.volley.Response\$Listener")
        val clsErrorListener = XpUtils.findClass(loadPackageParam.classLoader, "com.android.volley.Response\$ErrorListener")
        val clsAdInfo = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.util.AdUtils\$AdInfo")

        XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "isAdEnable", XC_MethodReplacement.returnConstant(false))
        if (clsListener != null && clsErrorListener != null) {
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "getPlayAd", clsListener, clsErrorListener, XC_MethodReplacement.returnConstant(null))
        }
        XpUtils.findAndHookMethod("com.miui.player.util.ExperimentsHelper", loadPackageParam.classLoader, "isAdEnabled", XC_MethodReplacement.returnConstant(false))
        if (clsAdInfo != null) {
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "handleDeepLinkUrl", Activity::class.java, clsAdInfo, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "showAlertAndDownload", Activity::class.java, clsAdInfo, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "handleAdClick", Activity::class.java, clsAdInfo, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.player.util.AdUtils", loadPackageParam.classLoader, "postPlayAdStat", String::class.java, clsAdInfo, XC_MethodReplacement.returnConstant(null))
        }
        XpUtils.findAndHookMethod("com.miui.player.phone.view.NowplayingAlbumPage", loadPackageParam.classLoader, "getPlayAd", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.player.util.Configuration", loadPackageParam.classLoader, "isCmTest", XC_MethodReplacement.returnConstant(true))
        XpUtils.findAndHookMethod("com.miui.player.util.Configuration", loadPackageParam.classLoader, "isCpLogoVisiable", XC_MethodReplacement.returnConstant(false))

        // fuck the ad under account
        val clsAdInfo2 = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.music.online.model.AdInfo")
        if (clsAdInfo != null) {
            XpUtils.findAndHookMethod("com.miui.player.util.AdForbidManager", loadPackageParam.classLoader, "recordAdInfo", clsAdInfo2, Integer.TYPE, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.player.util.AdForbidManager", loadPackageParam.classLoader, "addForbidInfo", String::class.java, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.player.util.AdForbidManager", loadPackageParam.classLoader, "isForbidden", String::class.java, XC_MethodReplacement.returnConstant(true))
        }
        XpUtils.findAndHookMethod("com.miui.player.hybrid.feature.GetAdInfo", loadPackageParam.classLoader, "addAdQueryParams", Context::class.java, Uri::class.java, XC_MethodReplacement.returnConstant(""))

        XpUtils.findAndHookMethod("com.miui.player.display.view.cell.BannerAdItemCell", loadPackageParam.classLoader, "onFinishInflate", object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val clsThis = param.thisObject.javaClass
                val vThis = param.thisObject as View
                val fText = clsThis.getDeclaredField("mClose")
                fText.isAccessible = true
                val vText = fText.get(param.thisObject) as View
                vText.visibility = View.GONE
                val fImg = clsThis.getDeclaredField("mImage")
                fImg.isAccessible = true
                val vImg = fImg.get(param.thisObject) as View
                vImg.visibility = View.GONE
                val lp = vThis.layoutParams
                lp.height = 0
                vThis.layoutParams = lp
            }
        })

        // try to remove copy right validation, removed.

        /*

        final Class<?> clsOnQualitySelectedListener = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.vip.DownloadVipHelper$OnQualitySelectedListener");
        final Class<?> clsResult = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.music.model.Result");
        if (clsOnQualitySelectedListener != null && clsResult != null) {
            XpUtils.findAndHookMethod("com.miui.player.vip.DownloadVipHelper", loadPackageParam.classLoader, "handleBatchDownloadPermission", Activity.class, List.class, int.class, boolean.class, clsOnQualitySelectedListener, clsResult, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    Object oResult = param.args[5];
                    Field fErrorCode = clsResult.getDeclaredField("mErrorCode");
                    fErrorCode.setAccessible(true);
                    fErrorCode.setInt(oResult, 1);
                }
            });
        }

        final Class<?> clsDownloadOne = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.download.MusicDownloader$DownloadOne");
        if (clsOnQualitySelectedListener != null && clsResult != null && clsDownloadOne != null) {
            XpUtils.findAndHookMethod("com.miui.player.vip.DownloadVipHelper", loadPackageParam.classLoader, "handleMusicUrl", Activity.class, clsDownloadOne, int.class, boolean.class, clsOnQualitySelectedListener, clsResult, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    Object oResult = param.args[5];
                    Field fErrorCode = clsResult.getDeclaredField("mErrorCode");
                    fErrorCode.setAccessible(true);
                    fErrorCode.setInt(oResult, 1);
                }
            });
        }

        // to be removed....
        if (clsDownloadOne != null) {
            XpUtils.findAndHookMethod("com.miui.player.vip.DownloadVipHelper", loadPackageParam.classLoader, "getMusicUrl", Activity.class, clsDownloadOne, int.class, new XC_MethodHook() {
                @Override
                protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                    Object oDownloadOne = param.args[1];
                    Field fValue = clsDownloadOne.getDeclaredField("mValue");
                    fValue.setAccessible(true);
                    Object oValue = fValue.get(oDownloadOne);
                    Field fGlobalId = oValue.getClass().getDeclaredField("mGlobalId");
                    fGlobalId.setAccessible(true);
                    String globalId = (String) fGlobalId.get(oValue);
                    try {
                        Class<?> clsEngineHelper = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.util.EngineHelper");
                        Method mGet = clsEngineHelper.getDeclaredMethod("get", Context.class);
                        Object oMusicEngine = mGet.invoke(null, param.args[0]);
                        Method mGetOnlineEngine = oMusicEngine.getClass().getDeclaredMethod("getOnlineEngine");
                        Object oOnlineEngine = mGetOnlineEngine.invoke(oMusicEngine);
                        Method mGetMusicLink = oOnlineEngine.getClass().getDeclaredMethod("getMusicLink", Context.class, String.class, int.class, int.class);

                        Class<?> clsGlobalId = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.content.GlobalIds");
                        Method mGetId = clsGlobalId.getDeclaredMethod("getId", String.class);
                        String newGlobalId = (String) mGetId.invoke(null, globalId);

                        Class<?> clsQualityUtils = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.player.util.QualityUtils");
                        Method mToBitrate = clsQualityUtils.getDeclaredMethod("toBitrate", int.class);
                        int bitrate = (int) mToBitrate.invoke(null, param.args[2]);

                        Object oResult = mGetMusicLink.invoke(oOnlineEngine, param.args[0], newGlobalId, bitrate, 1);
                        // XposedBridge.log(String.format("getMusicUrl => %s", oResult.toString()));

                    } catch (Exception e) {

                    }
                }
            });
        }
        XpUtils.findAndHookMethod("com.xiaomi.music.online.model.BatchDownloadPermission", loadPackageParam.classLoader, "allDownloadAllowed", XC_MethodReplacement.returnConstant(true));

        */
    }

}
