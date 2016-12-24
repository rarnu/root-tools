package com.rarnu.tools.neo.xposed.ads

import android.app.Activity
import android.content.Context
import android.net.Uri
import android.view.View
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.XposedHelpers
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
                val vThis = param.thisObject as View
                try {
                    (XposedHelpers.getObjectField(param.thisObject, "mClose") as View?)?.visibility = View.GONE
                } catch (t: Throwable) {
                }
                try {
                    (XposedHelpers.getObjectField(param.thisObject, "mImage") as View?)?.visibility = View.GONE
                } catch (t: Throwable) {
                }
                val lp = vThis.layoutParams
                lp.height = 0
                vThis.layoutParams = lp
            }
        })

        // 2.7.300
        XpUtils.findAndHookMethod("com.miui.player.content.MusicHybridProvider", loadPackageParam.classLoader, "parseCommand", String::class, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                val scheme = param.args[0] as String?
                if (scheme == "advertise") {
                    param.args[0] = ""
                }
            }
        })

        XpUtils.findAndHookMethod("com.miui.systemAdSolution.landingPage.LandingPageService", loadPackageParam.classLoader, "init", Context::class.java, XC_MethodReplacement.returnConstant(null))

        // 2.7.400
        XpUtils.findAndHookMethod("com.miui.player.phone.view.NowplayingContentView", loadPackageParam.classLoader, "setInfoVisibility", java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                param.args[0] = true
            }
        })


        if (clsAdInfo2 != null) {
            XpUtils.findAndHookConstructor("com.miui.player.phone.view.NowplayingContentView\$ShowAdRunnable", loadPackageParam.classLoader, clsAdInfo2, java.lang.Boolean.TYPE, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    param.args[1] = false
                }
            })
        }

        XpUtils.findAndHookMethod("com.miui.player.phone.view.NowplayingContentView\$ShowAdRunnable", loadPackageParam.classLoader, "setLoadAd", java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                param.args[0] = false
            }
        })


    }

}
