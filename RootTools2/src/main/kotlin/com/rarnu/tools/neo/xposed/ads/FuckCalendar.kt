package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.content.Intent
import com.rarnu.xfunc.*
import org.json.JSONObject

/**
 * Created by rarnu on 11/18/16.
 */
object FuckCalendar {

    fun fuckCalendar(pkg: XposedPkg) {
        pkg.findClass("com.miui.calendar.ad.AdUtils").apply {
            findMethod("canShowBrandAd", Context::class.java).hook { replace { result = false } }
            findMethod("getActionBarBitmap", Context::class.java).hook { replace { result = null } }
            findMethod("getAdConfigJson", Context::class.java).hook { replace { result = null } }
        }
        pkg.findClass("com.miui.calendar.ad.AdService").findMethod("onHandleIntent", Intent::class.java).hook { replace { result = null } }
        pkg.findClass("com.miui.calendar.card.single.local.SummarySingleCard").findMethod("needShowAdBanner").hook { replace { result = false } }
        pkg.findClass("com.miui.calendar.card.single.custom.ad.AdSingleCard").findMethod("needDisplay").hook { replace { result = false } }
        pkg.findClass("com.miui.calendar.card.single.custom.HistorySingleCard").findMethod("needDisplay").hook { replace { result = false } }
        pkg.findClass("com.miui.calendar.huangli.HuangLiDetailActivity").findMethod("startLoading").hook { replace { result = null } }
        pkg.findClass("com.miui.calendar.card.single.custom.RecommendSingleCard").findMethod("needDisplay").hook { replace { result = false } }
        pkg.findClass("com.miui.calendar.card.single.custom.ad.LargeImageAdSingleCard").findMethod("needDisplay").hook { replace { result = false } }

        val clsC = pkg.findClass("com.xiaomi.ad.internal.common.module.a\$c")
        if (clsC != null) {
            pkg.findClass("com.xiaomi.ad.internal.common.module.a").findMethod("b", clsC).hook { replace { result = null } }
        }
        pkg.findClass("com.xiaomi.ad.common.pojo.Ad").findMethod("parseJson", JSONObject::class.java).hook { replace { result = null } }
        pkg.findClass("com.xiaomi.ad.internal.a.e").findMethod("onAdInfo", String::class.java).hook { replace { result = null } }
        pkg.findClass("com.miui.calendar.util.DiskStringCache").findMethod("getString", Context::class.java, String::class.java).hook {
            before {
                val key = args[1] as String
                if (key.startsWith("bottom_banner_is_closed_today")) {
                    result = "true"
                }
            }
            after {
                val key = args[1] as String
                if (key.startsWith("bottom_banner_is_closed_today")) {
                    result = "true"
                }
            }
        }
    }
}
