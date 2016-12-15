package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.content.Loader
import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.FrameLayout
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage
import org.json.JSONObject
import java.util.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckVideo {

    fun fuckVideo(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("com.miui.videoplayer.ads.DynamicAd", loadPackageParam.classLoader, "replaceList", List::class.java, String::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                paramAnonymousMethodHookParam.args[0] = null
                paramAnonymousMethodHookParam.args[1] = null
            }
        })
        XpUtils.findAndHookMethod("com.video.ui.view.AdView", loadPackageParam.classLoader, "getAdsBlock", Context::class.java, XC_MethodReplacement.returnConstant(null))
        val clsCallback = XpUtils.findClass(loadPackageParam.classLoader, "com.video.ui.idata.SyncServiceHelper\$Callback")
        if (clsCallback != null) {
            XpUtils.findAndHookMethod("com.video.ui.idata.SyncServiceHelper", loadPackageParam.classLoader, "fetchAds", Context::class.java, clsCallback, XC_MethodReplacement.returnConstant(null))
        }
        XpUtils.findAndHookMethod("com.video.ui.idata.iDataORM", loadPackageParam.classLoader, "getBooleanValue", Context::class.java, String::class.java, java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val key = param.args[1] as String
                if (key == "debug_mode" || key == "show_first_ads" || key == "ads_show_homekey" || key == "startup_ads_loop" || key == "app_upgrade_splash") {
                    param.result = false
                }
            }

            @Throws(Throwable::class)
            override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val key = param.args[1] as String
                if (key == "debug_mode" || key == "show_first_ads" || key == "ads_show_homekey" || key == "startup_ads_loop" || key == "app_upgrade_splash") {
                    param.result = false
                }
            }
        })
        XpUtils.findAndHookMethod("com.video.ui.idata.iDataORM", loadPackageParam.classLoader, "getStringValue", Context::class.java, String::class.java, String::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val key = param.args[1] as String
                if (key == "startup_ads") {
                    param.result = null
                }
            }

            @Throws(Throwable::class)
            override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val key = param.args[1] as String
                if (key == "startup_ads") {
                    param.result = null
                }
            }
        })
        XpUtils.findAndHookMethod("com.video.ui.idata.iDataORM", loadPackageParam.classLoader, "getBooleanValue", Context::class.java, String::class.java, java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val key = param.args[1] as String
                if (key == "show_title_ads" || key == "show_channel_title_ads") {
                    param.result = false
                }
            }

            @Throws(Throwable::class)
            override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val key = param.args[1] as String
                if (key == "show_title_ads" || key == "show_channel_title_ads") {
                    param.result = false
                }
            }
        })
        XpUtils.findAndHookMethod("com.video.ui.idata.iDataORM", loadPackageParam.classLoader, "enabledAds", Context::class.java, XC_MethodReplacement.returnConstant(false))
        val clsAdListener = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.systemAdSolution.splashAd.IAdListener")
        if (clsAdListener != null) {
            XpUtils.findAndHookMethod("com.miui.ad.sdk.api.RemoteSystemSplashAdService", loadPackageParam.classLoader, "requestSystemSplashAd", clsAdListener, XC_MethodReplacement.returnConstant(false))
            XpUtils.findAndHookMethod("com.miui.ad.sdk.api.SystemSplashAd", loadPackageParam.classLoader, "requestAd", Context::class.java, clsAdListener, XC_MethodReplacement.returnConstant(null))
            XpUtils.findAndHookMethod("com.miui.ad.sdk.api.SystemSplashAd", loadPackageParam.classLoader, "requestAd", clsAdListener, XC_MethodReplacement.returnConstant(null))
        }

        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "supportFrontAD", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "supportPauseAD", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "supportCornerAD", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "skipAllAD", XC_MethodReplacement.returnConstant(true))
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "skipSDKAD", XC_MethodReplacement.returnConstant(true))
        XpUtils.findAndHookMethod("com.miui.videoplayer.model.OnlineUri", loadPackageParam.classLoader, "getMiAdFlag", XC_MethodReplacement.returnConstant(-1))

        XpUtils.findAndHookMethod("com.miui.videoplayer.ads.AdsContainer", loadPackageParam.classLoader, "setCornerAd", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.videoplayer.ads.AdsContainer", loadPackageParam.classLoader, "enableOfflineAds", XC_MethodReplacement.returnConstant(false))

        XpUtils.findAndHookMethod("com.miui.videoplayer.videoview.VideoViewContainer", loadPackageParam.classLoader, "playAd", XC_MethodReplacement.returnConstant(null))
        val clsVideoView = XpUtils.findClass(loadPackageParam.classLoader, "com.miui.videoplayer.videoview.IVideoView")
        if (clsVideoView != null) {
            XpUtils.findAndHookMethod("com.miui.videoplayer.videoview.VideoViewContainer", loadPackageParam.classLoader, "playRealVideo", clsVideoView, java.lang.Boolean.TYPE, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                    param.args[1] = true   // skipAd
                }
            })
        }

        XpUtils.findAndHookMethod("com.miui.videoplayer.videoview.VideoViewContainer", loadPackageParam.classLoader, "prepareRealVideoView", java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                param.args[0] = true  // haveAd
            }
        })

        XpUtils.findAndHookMethod("com.miui.videoplayer.ads.AdsService", loadPackageParam.classLoader, "doLaunch", String::class.java, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.miui.videoplayer.videoview.MiAdsVideoView", loadPackageParam.classLoader, "haveAd", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.xiaomi.miui.ad.listeners.impl.AdEventListenerImpl", loadPackageParam.classLoader, "onAdRequest", String::class.java, JSONObject::class.java, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.xiaomi.miui.ad.listeners.impl.AdEventListenerImpl", loadPackageParam.classLoader, "onAdClicked", String::class.java, JSONObject::class.java, XC_MethodReplacement.returnConstant(null))

        val clsBlock = XpUtils.findClass(loadPackageParam.classLoader, "com.tv.ui.metro.model.Block")
        val clsPool = XpUtils.findClass(loadPackageParam.classLoader, "android.support.v7.widget.RecyclerView\$RecycledViewPool")
        if (clsBlock != null && clsPool != null) {
            XpUtils.findAndHookConstructor("com.video.ui.view.BlockAdapter", loadPackageParam.classLoader, Context::class.java, clsBlock, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam?) {
                    removeAds(param)
                }
            })
            XpUtils.findAndHookConstructor("com.video.ui.view.BlockAdapter", loadPackageParam.classLoader, Context::class.java, clsBlock, clsPool, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam?) {
                    removeAds(param)
                }
            })
            XpUtils.findAndHookMethod("com.video.ui.view.BlockAdapter", loadPackageParam.classLoader, "addGroup", clsBlock, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam?) {
                    removeAds(param)
                }
            })
            XpUtils.findAndHookMethod("com.video.ui.view.block.PortBlockView", loadPackageParam.classLoader, "initUI", clsBlock, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                    val block = param.args[0]
                    val clsBlock2 = block.javaClass
                    val fBlocks = clsBlock2.getDeclaredField("blocks")
                    fBlocks.isAccessible = true
                    val blocks = fBlocks.get(block) as ArrayList<*>
                    for (i in blocks.indices.reversed()) {
                        val o = blocks[i]
                        val fUI = o.javaClass.superclass.getDeclaredField("ui_type")
                        val oUI = fUI.get(o)
                        val mId = oUI.javaClass.getMethod("id")
                        val id = mId.invoke(oUI) as Int
                        if (id == 101 || id == 10001 || id == 282 || id == 257 || id == 221 ||
                                id == 501 || id == 502 || id == 503 || (id == 601) or (id == 602) || id == 603 || id == 604) {
                            blocks.removeAt(i)
                        }
                    }
                }
            })
        }

        if (clsBlock != null) {
            XpUtils.findAndHookMethod("com.video.ui.view.ListFragment", loadPackageParam.classLoader, "setBlockView", clsBlock, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                    val o = param.args[0]
                    val fBlocks = o.javaClass.getDeclaredField("blocks")
                    fBlocks.isAccessible = true
                    fBlocks.set(o, null)
                    val fFooters = o.javaClass.getDeclaredField("footers")
                    fFooters.isAccessible = true
                    fFooters.set(o, null)
                }
            })
        }

        val clsVideo = XpUtils.findClass(loadPackageParam.classLoader, "com.tv.ui.metro.model.VideoItem")
        if (clsVideo != null) {
            XpUtils.findAndHookMethod("com.video.ui.view.DetailFragment", loadPackageParam.classLoader, "updateVideo", clsVideo, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                    val oVideo = param.args[0]
                    val fBlocks = oVideo.javaClass.getDeclaredField("blocks")
                    fBlocks.isAccessible = true
                    fBlocks.set(oVideo, null)
                    val fHeaders = oVideo.javaClass.getDeclaredField("headers")
                    fHeaders.isAccessible = true
                    fHeaders.set(oVideo, null)
                }
            })
        }

        XpUtils.findAndHookMethod("com.video.ui.view.DetailFragment", loadPackageParam.classLoader, "onCreateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun afterHookedMethod(param: XC_MethodHook.MethodHookParam) {
                val fragment = param.thisObject
                val fR1 = fragment.javaClass.getDeclaredField("relative_region")
                val fR2 = fragment.javaClass.getDeclaredField("headers_region")
                val r1 = fR1.get(fragment) as FrameLayout
                val r2 = fR2.get(fragment) as FrameLayout
                r1.visibility = View.GONE
                r2.visibility = View.GONE
            }
        })
        XpUtils.findAndHookMethod("com.video.ui.view.DetailFragment", loadPackageParam.classLoader, "checkAdsPresentVisibility", View::class.java, XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("com.video.ui.view.ListFragment", loadPackageParam.classLoader, "checkAdsPresentVisibility", View::class.java, XC_MethodReplacement.returnConstant(null))

        val clsGenericBlock = XpUtils.findClass(loadPackageParam.classLoader, "com.tv.ui.metro.model.GenericBlock")
        if (clsGenericBlock != null) {
            XpUtils.findAndHookMethod("com.video.ui.view.user.MyVideoFragment", loadPackageParam.classLoader, "onLoadFinished", Loader::class.java, clsGenericBlock, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                    param.args[1] = null
                }
            })
        }
    }

    private fun removeAds(param: XC_MethodHook.MethodHookParam?) {
        try {
            val clsThis = param?.thisObject?.javaClass
            val fBlockRootArrayList = clsThis?.getDeclaredField("mBlockRootArrayList")
            fBlockRootArrayList?.isAccessible = true
            val mBlockRootArrayList = fBlockRootArrayList?.get(param?.thisObject) as ArrayList<*>
            for (i in mBlockRootArrayList.indices.reversed()) {
                val b = mBlockRootArrayList[i]
                val fUI = b.javaClass.superclass.getDeclaredField("ui_type")
                fUI.isAccessible = true
                val oUI = fUI.get(b)
                val mId = oUI.javaClass.getMethod("id")
                val id = mId.invoke(oUI) as Int
                if (id == 101 || id == 10001 || id == 282 || id == 257 || id == 221 ||
                        id == 501 || id == 502 || id == 503 || (id == 601) or (id == 602) || id == 603 || id == 604) {
                    mBlockRootArrayList.removeAt(i)
                }
            }
        } catch (e: Exception) {
        }

    }
}
