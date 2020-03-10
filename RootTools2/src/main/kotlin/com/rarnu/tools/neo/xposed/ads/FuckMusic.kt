@file:Suppress("Duplicates")

package com.rarnu.tools.neo.xposed.ads

import android.app.Activity
import android.content.Context
import android.net.Uri
import android.util.AttributeSet
import android.view.View
import com.rarnu.xfunc.*
import de.robv.android.xposed.XposedHelpers

/**
 * Created by rarnu on 11/18/16.
 */
object FuckMusic {

    fun fuckMusic(pkg: XposedPkg) {
        val clsListener = pkg.findClass("com.android.volley.Response\$Listener")
        val clsErrorListener = pkg.findClass("com.android.volley.Response\$ErrorListener")
        val clsAdInfo = pkg.findClass("com.miui.player.util.AdUtils\$AdInfo")
        pkg.findClass("com.miui.player.util.AdUtils").apply {
            findMethod("isAdEnable").hook { replace { result = false } }
            if (clsListener != null && clsErrorListener != null) {
                findMethod("getPlayAd", clsListener, clsErrorListener).hook { replace { result = null } }
            }
            if (clsAdInfo != null) {
                findMethod("handleDeepLinkUrl", Activity::class.java, clsAdInfo).hook { replace { result = null } }
                findMethod("showAlertAndDownload", Activity::class.java, clsAdInfo).hook { replace { result = null } }
                findMethod("handleAdClick", Activity::class.java, clsAdInfo).hook { replace { result = null } }
                findMethod("postPlayAdStat", String::class.java, clsAdInfo).hook { replace { result = null } }
            }
        }

        pkg.findClass("com.miui.player.util.ExperimentsHelper").findMethod("isAdEnabled").hook { replace { result = null } }
        pkg.findClass("com.miui.player.phone.view.NowplayingAlbumPage").findMethod("getPlayAd").hook { replace { result = null } }
        pkg.findClass("com.miui.player.util.Configuration").apply {
            findMethod("isCmTest").hook { replace { result = true } }
            findMethod("isCpLogoVisiable").hook { replace { result = false } }
        }

        // fuck the ad under account
        val clsAdInfo2 = pkg.findClass("com.xiaomi.music.online.model.AdInfo")
        if (clsAdInfo != null) {
            pkg.findClass("com.miui.player.util.AdForbidManager").apply {
                findMethod("recordAdInfo", clsAdInfo2, Integer.TYPE).hook { replace { result = null } }
                findMethod("addForbidInfo", String::class.java).hook { replace { result = null } }
                findMethod("isForbidden", String::class.java).hook { replace { result = true } }
            }
        }
        pkg.findClass("com.miui.player.hybrid.feature.GetAdInfo").findMethod("addAdQueryParams", Context::class.java, Uri::class.java).hook { replace { result = "" } }
        pkg.findClass("com.miui.player.display.view.cell.BannerAdItemCell").findMethod("onFinishInflate").hook {
            after {
                val vThis = thisObject as View
                try {
                    (XposedHelpers.getObjectField(thisObject, "mClose") as View?)?.visibility = View.GONE
                } catch (t: Throwable) {
                }
                try {
                    (XposedHelpers.getObjectField(thisObject, "mImage") as View?)?.visibility = View.GONE
                } catch (t: Throwable) {
                }
                vThis.layoutParams = vThis.layoutParams.apply { height = 0 }
            }
        }

        // 2.7.300
        pkg.findClass("com.miui.player.content.MusicHybridProvider").findMethod("parseCommand", String::class.java).hook {
            before {
                val scheme = args[0] as String?
                if (scheme == "advertise") {
                    args[0] = ""
                }
            }
        }
        pkg.findClass("com.miui.systemAdSolution.landingPage.LandingPageService").findMethod("init", Context::class.java).hook { replace { result = null } }

        // 2.7.400
        pkg.findClass("com.miui.player.phone.view.NowplayingContentView").findMethod("setInfoVisibility", java.lang.Boolean.TYPE).hook {
            before {
                args[0] = true
            }
        }

        if (clsAdInfo2 != null) {
            pkg.findClass("com.miui.player.phone.view.NowplayingContentView\$ShowAdRunnable").findConstructor(clsAdInfo2, java.lang.Boolean.TYPE).hook {
                before {
                    args[1] = false
                }
            }
        }
        pkg.findClass("com.miui.player.phone.view.NowplayingContentView\$ShowAdRunnable").findMethod("setLoadAd", java.lang.Boolean.TYPE).hook {
            before {
                args[0] = false
            }
        }

        // 2.8
        val clsDisplayItem = pkg.findClass("com.miui.player.display.model.DisplayItem")
        if (clsDisplayItem != null) {
            pkg.findClass("com.miui.player.display.view.SearchPopularKeyCard").findMethod("onBindItem", clsDisplayItem, Integer.TYPE).hook {
                after {
                    (thisObject as View).apply {
                        layoutParams.height = 0
                        visibility = View.GONE
                    }
                }
            }
            pkg.findClass("com.miui.player.display.view.BannerCard").findMethod("onBindItem", clsDisplayItem, Integer.TYPE).hook {
                after {
                    (thisObject as View).apply {
                        layoutParams.height = 0
                        visibility = View.GONE
                    }
                }
            }
        }

        val clsAdShowingListener = pkg.findClass("com.miui.player.phone.view.NowplayingAlbumPage\$AdShowingListener")
        if (clsAdShowingListener != null) {
            pkg.findClass("com.miui.player.phone.view.NowplayingAlbumView").findMethod("setAdShowingListener", clsAdShowingListener).hook { replace { result = null } }
        }
        pkg.findClass("com.miui.player.phone.view.NowplayingAlbumPage\$ShowAdRunnable").findConstructor(java.lang.Boolean.TYPE).hook {
            before {
                args[0] = false
            }
        }

        // 2.9.0
        pkg.findClass("com.miui.player.content.preference.PreferenceCache").findMethod("getBoolean", Context::class.java, String::class.java).hook {
            after {
                if (args[1] == "ad_recommend") {
                    result = false
                }
            }
        }
        pkg.findClass("com.miui.player.util.ExperimentsHelper").findMethod("isAdEnabled").hook { replace { result = false } }
        pkg.findClass("com.miui.player.phone.view.NowplayingAlbumPage").findConstructor(Context::class.java, AttributeSet::class.java, Integer.TYPE).hook {
            after {
                val mAdMark = XposedHelpers.getObjectField(thisObject, "mAdMark") as View?
                mAdMark?.visibility = View.GONE
            }
        }
    }

}
