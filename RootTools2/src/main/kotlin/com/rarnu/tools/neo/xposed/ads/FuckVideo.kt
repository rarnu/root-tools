@file:Suppress("RECEIVER_NULLABILITY_MISMATCH_BASED_ON_JAVA_ANNOTATIONS", "DEPRECATION", "Duplicates")

package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.content.Loader
import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.FrameLayout
import com.rarnu.xfunc.*
import de.robv.android.xposed.XC_MethodHook
import org.json.JSONObject
import java.util.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckVideo {

    fun fuckVideo(pkg: XposedPkg) {
        pkg.findClass("com.miui.videoplayer.ads.DynamicAd").findMethod("replaceList", List::class.java, String::class.java).hook {
            before {
                args[0] = null
                args[1] = null
            }
        }
        pkg.findClass("com.video.ui.view.AdView").findMethod("getAdsBlock", Context::class.java).hook { replace { result = null } }

        val clsCallback = pkg.findClass("com.video.ui.idata.SyncServiceHelper\$Callback")
        if (clsCallback != null) {
            pkg.findClass("com.video.ui.idata.SyncServiceHelper").findMethod("fetchAds", Context::class.java, clsCallback).hook { replace { result = null } }
        }
        pkg.findClass("com.video.ui.idata.iDataORM").findMethod("getBooleanValue", Context::class.java, String::class.java, java.lang.Boolean.TYPE).hook {
            before {
                val key = args[1] as String
                if (key == "debug_mode" || key == "show_first_ads" || key == "ads_show_homekey" || key == "startup_ads_loop" || key == "app_upgrade_splash") {
                    result = false
                }
            }
            after {
                val key = args[1] as String
                if (key == "debug_mode" || key == "show_first_ads" || key == "ads_show_homekey" || key == "startup_ads_loop" || key == "app_upgrade_splash") {
                    result = false
                }
            }
        }
        pkg.findClass("com.video.ui.idata.iDataORM").findMethod("getStringValue", Context::class.java, String::class.java, String::class.java).hook {
            before {
                val key = args[1] as String
                if (key == "startup_ads") {
                    result = null
                }
            }
            after {
                val key = args[1] as String
                if (key == "startup_ads") {
                    result = null
                }
            }
        }
        pkg.findClass("com.video.ui.idata.iDataORM").findMethod("getBooleanValue", Context::class.java, String::class.java, java.lang.Boolean.TYPE).hook {
            before {
                val key = args[1] as String
                if (key == "show_title_ads" || key == "show_channel_title_ads") {
                    result = false
                }
            }
            after {
                val key = args[1] as String
                if (key == "show_title_ads" || key == "show_channel_title_ads") {
                    result = false
                }
            }
        }
        pkg.findClass("com.video.ui.idata.iDataORM").findMethod("enabledAds", Context::class.java).hook { replace { result = false } }

        val clsAdListener = pkg.findClass("com.miui.systemAdSolution.splashAd.IAdListener")
        if (clsAdListener != null) {
            pkg.findClass("com.miui.ad.sdk.api.RemoteSystemSplashAdService").findMethod("requestSystemSplashAd", clsAdListener).hook { replace { result = false } }
            pkg.findClass("com.miui.ad.sdk.api.SystemSplashAd").apply {
                findMethod("requestAd", Context::class.java, clsAdListener).hook { replace { result = null } }
                findMethod("requestAd", clsAdListener).hook { replace { result = null } }
            }
        }

        pkg.findClass("com.miui.videoplayer.model.OnlineUri").apply {
            findMethod("supportFrontAD").hook { replace { result = false } }
            findMethod("supportPauseAD").hook { replace { result = false } }
            findMethod("supportCornerAD").hook { replace { result = false } }
            findMethod("skipAllAD").hook { replace { result = true } }
            findMethod("skipSDKAD").hook { replace { result = true } }
            findMethod("getMiAdFlag").hook { replace { result = -1 } }
        }

        pkg.findClass("com.miui.videoplayer.ads.AdsContainer").apply {
            findMethod("setCornerAd").hook { replace { result = null } }
            findMethod("enableOfflineAds").hook { replace { result = false } }
        }
        pkg.findClass("com.miui.videoplayer.videoview.VideoViewContainer").findMethod("playAd").hook { replace { result = null } }

        val clsVideoView = pkg.findClass("com.miui.videoplayer.videoview.IVideoView")
        if (clsVideoView != null) {
            pkg.findClass("com.miui.videoplayer.videoview.VideoViewContainer").findMethod("playRealVideo", clsVideoView, java.lang.Boolean.TYPE).hook {
                before {
                    args[1] = true   // skipAd
                }
            }
        }

        pkg.findClass("com.miui.videoplayer.videoview.VideoViewContainer").findMethod("prepareRealVideoView", java.lang.Boolean.TYPE).hook {
            before {
                args[0] = true  // haveAd
            }
        }

        // TODO:
        pkg.findClass("com.miui.videoplayer.ads.AdsService").findMethod("doLaunch", String::class.java).hook { replace { result = null } }
        pkg.findClass("com.miui.videoplayer.videoview.MiAdsVideoView").findMethod("haveAd").hook { replace { result = false } }
        pkg.findClass("com.xiaomi.miui.ad.listeners.impl.AdEventListenerImpl").apply {
            findMethod("onAdRequest", String::class.java, JSONObject::class.java).hook { replace { result = null } }
            findMethod("onAdClicked", String::class.java, JSONObject::class.java).hook { replace { result = null } }
        }

        val clsBlock = pkg.findClass("com.tv.ui.metro.model.Block")
        val clsPool = pkg.findClass("android.support.v7.widget.RecyclerView\$RecycledViewPool")
        if (clsBlock != null && clsPool != null) {
            pkg.findClass("com.video.ui.view.BlockAdapter").apply {
                findConstructor(Context::class.java, clsBlock).hook {
                    after { removeAds(this) }
                }
                findConstructor(Context::class.java, clsBlock, clsPool).hook {
                    after { removeAds(this) }
                }
                findMethod("addGroup", clsBlock).hook {
                    after { removeAds(this) }
                }
            }

            pkg.findClass("com.video.ui.view.block.PortBlockView").findMethod("initUI", clsBlock).hook {
                before {
                    val block = args[0]
                    val clsBlock2 = block.javaClass
                    val fBlocks = clsBlock2.getDeclaredField("blocks").apply { isAccessible = true }
                    val blocks = fBlocks.get(block) as ArrayList<*>
                    blocks.indices.reversed().forEach {
                        val o = blocks[it]
                        val fUI = o.javaClass.superclass.getDeclaredField("ui_type")
                        val oUI = fUI.get(o)
                        val mId = oUI.javaClass.getMethod("id")
                        val id = mId.invoke(oUI) as Int
                        if (id == 101 || id == 10001 || id == 282 || id == 257 || id == 221 ||
                                id == 501 || id == 502 || id == 503 || (id == 601) or (id == 602) || id == 603 || id == 604) {
                            blocks.removeAt(it)
                        }
                    }
                }
            }
        }

        if (clsBlock != null) {
            pkg.findClass("com.video.ui.view.ListFragment").findMethod("setBlockView", clsBlock).hook {
                before {
                    val o = args[0]
                    val fBlocks = o.javaClass.getDeclaredField("blocks").apply { isAccessible = true }
                    fBlocks.set(o, null)
                    val fFooters = o.javaClass.getDeclaredField("footers").apply { isAccessible = true }
                    fFooters.set(o, null)
                }
            }
        }

        val clsVideo = pkg.findClass("com.tv.ui.metro.model.VideoItem")
        if (clsVideo != null) {
            pkg.findClass("com.video.ui.view.DetailFragment").findMethod("updateVideo", clsVideo).hook {
                before {
                    val oVideo = args[0]
                    val fBlocks = oVideo.javaClass.getDeclaredField("blocks").apply { isAccessible = true }
                    fBlocks.set(oVideo, null)
                    val fHeaders = oVideo.javaClass.getDeclaredField("headers").apply { isAccessible = true }
                    fHeaders.set(oVideo, null)
                }
            }
        }

        pkg.findClass("com.video.ui.view.DetailFragment").apply {
            findMethod("onCreateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java).hook {
                after {
                    val fragment = thisObject
                    val fR1 = fragment.javaClass.getDeclaredField("relative_region")
                    val fR2 = fragment.javaClass.getDeclaredField("headers_region")
                    val r1 = fR1.get(fragment) as FrameLayout
                    val r2 = fR2.get(fragment) as FrameLayout
                    r1.visibility = View.GONE
                    r2.visibility = View.GONE
                }
            }
            findMethod("checkAdsPresentVisibility", View::class.java).hook { replace { result = null } }
        }

        pkg.findClass("com.video.ui.view.ListFragment").findMethod("checkAdsPresentVisibility", View::class.java).hook { replace { result = null } }

        val clsGenericBlock = pkg.findClass("com.tv.ui.metro.model.GenericBlock")
        if (clsGenericBlock != null) {
            pkg.findClass("com.video.ui.view.user.MyVideoFragment").findMethod("onLoadFinished", Loader::class.java, clsGenericBlock).hook {
                before {
                    args[1] = null
                }
            }
        }

        pkg.findClass("com.miui.systemAdSolution.landingPage.LandingPageService").findMethod("init", Context::class.java).hook { replace { result = null } }

    }

    private fun removeAds(param: XC_MethodHook.MethodHookParam) {
        try {
            val clsThis = param.thisObject?.javaClass
            val fBlockRootArrayList = clsThis?.getDeclaredField("mBlockRootArrayList")?.apply { isAccessible = true }
            val mBlockRootArrayList = fBlockRootArrayList?.get(param.thisObject) as ArrayList<*>

            mBlockRootArrayList.indices.reversed().forEach {
                val b = mBlockRootArrayList[it]
                val fUI = b.javaClass.superclass.getDeclaredField("ui_type").apply { isAccessible = true }
                val oUI = fUI.get(b)
                val mId = oUI.javaClass.getMethod("id")
                val id = mId.invoke(oUI) as Int
                if (id == 101 || id == 10001 || id == 282 || id == 257 || id == 221 ||
                        id == 501 || id == 502 || id == 503 || (id == 601) or (id == 602) || id == 603 || id == 604) {
                    mBlockRootArrayList.removeAt(it)
                }
            }
        } catch (e: Exception) {
        }

    }
}
