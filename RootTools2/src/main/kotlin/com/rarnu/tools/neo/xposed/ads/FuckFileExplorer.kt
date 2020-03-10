package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import com.rarnu.xfunc.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckFileExplorer {

    fun fuckFileExplorer(pkg: XposedPkg) {
        pkg.findClass("com.android.fileexplorer.model.ConfigHelper").apply {
            findMethod("isAdEnable", Context::class.java, String::class.java).hook { replace { result = false } }
            findMethod("supportAd").hook { replace { result = false } }
            findMethod("ifAdShowByCloudForNetwork", Context::class.java, String::class.java).hook { replace { result = false } }
            findMethod("getHomePageHotVideoTipSwitch", Context::class.java).hook { replace { result = false } }
            findMethod("getHomePageHotVideoTopicUri", Context::class.java).hook { replace { result = "" } }
            findMethod("getAdStyleName", Context::class.java, String::class.java).hook { replace { result = "" } }
            findMethod("tryInit", Context::class.java).hook { replace { result = false } }
            findMethod("isVideoEnable", Context::class.java).hook { replace { result = false } }
            findMethod("isStickerEnable", Context::class.java).hook { replace { result = false } }
        }
        pkg.findClass("com.android.fileexplorer.util.XLUtil").findMethod("isNetworkAvailable", Context::class.java).hook { replace { result = false } }
        pkg.findClass("com.xunlei.adlibrary.XunleiADSdk").findMethod("setup", Context::class.java).hook { replace { result = null } }
        pkg.findClass("com.xunlei.adlibrary.analytics.xunlei.AdStatHelper").findMethod("init", Context::class.java).hook { replace { result = null } }
        pkg.findClass("com.android.fileexplorer.video.upload.VideoItemManager").findMethod("initLoad").hook { replace { result = null } }
    }

}
