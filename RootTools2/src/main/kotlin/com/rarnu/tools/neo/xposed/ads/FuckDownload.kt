package com.rarnu.tools.neo.xposed.ads

import com.rarnu.xfunc.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckDownload {

    fun fuckDownload(pkg: XposedPkg) {
        pkg.findClass("com.android.providers.downloads.ui.recommend.config.ADConfig").findMethod("OSSupportAD").hook { replace { result = false } }
        pkg.findClass("com.android.providers.downloads.ui.utils.CloudConfigHelper").apply {
            findMethod("supportRank").hook { replace { result = false } }
            findMethod("isShouldShowAd").hook { replace { result = false } }
            findMethod("isShouldShowRecommendInfo").hook { replace { result = false } }
            findMethod("getHomeAdRefreshType").hook { replace { result = 0 } }
            findMethod("isShouldShowExtraAd").hook { replace { result = false } }
            findMethod("isShouldShowRankGuide").hook { replace { result = false } }
        }
    }
}
