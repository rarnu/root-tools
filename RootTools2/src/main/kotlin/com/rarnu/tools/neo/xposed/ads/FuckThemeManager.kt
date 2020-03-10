package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import com.rarnu.xfunc.*

/**
 * Created by rarnu on 11/26/16.
 */
object FuckThemeManager {

    fun fuckThemeManager(pkg: XposedPkg) {
        val clsPageItem = pkg.findClass("com.android.thememanager.model.PageItem")
        if (clsPageItem != null) {
            pkg.findClass("com.android.thememanager.controller.online.PageItemViewConverter").findMethod("buildAdView", clsPageItem).hook { replace { result = null } }
        }

        val clsHybridView = pkg.findClass("miui.hybrid.HybridView")
        if (clsHybridView != null) {
            pkg.findClass("com.android.thememanager.h5.ThemeHybridFragment\$BaseWebViewClient").findMethod("shouldInterceptRequest", clsHybridView, String::class.java).hook {
                before {
                    val url = args[1] as String
                    if (url.contains("AdCenter")) {
                        args[1] = "http://127.0.0.1/"
                    }
                }
            }
        }

        // 0.8
        pkg.findClass("com.android.thememanager.util.ApplicationHelper").apply {
            findMethod("isFreshMan").hook { replace { result = false } }
            findMethod("hasFreshManMarkRecord", Context::class.java).hook { replace { result = false } }
        }

        pkg.findClass("com.miui.systemAdSolution.landingPage.LandingPageService").findMethod("init", Context::class.java).hook { replace { result = null } }

    }
}
