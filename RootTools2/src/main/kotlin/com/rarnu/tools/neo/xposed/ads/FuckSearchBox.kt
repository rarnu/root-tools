package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import com.rarnu.xfunc.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckSearchBox {

    fun fuckSearchBox(pkg: XposedPkg) {
        pkg.findClass("com.android.quicksearchbox.ui.LocalListView").apply {
            findMethod("updateHotQuery", List::class.java, Integer.TYPE).hook { replace { result = null } }
            findMethod("updateHotQuery", List::class.java, Integer.TYPE, java.lang.Boolean.TYPE).hook { replace { result = null } }
            findMethod("updateBanners").hook { replace { result = null } }
        }
        pkg.findClass("com.android.quicksearchbox.util.Util").findMethod("isShowHotView", Context::class.java).hook { replace { result = false } }

        val clsQueryView = pkg.findClass("com.android.quicksearchbox.ui.HotQueryView")
        if (clsQueryView != null) {
            pkg.findClass("com.android.quicksearchbox.util.HotWordsUtil").findMethod("setHotQueryView", clsQueryView).hook { replace { result = null } }
        }
        pkg.findClass("com.android.quicksearchbox.SearchSettingsImpl").findMethod("getShowHot").hook { replace { result = false } }
        pkg.findClass("com.android.quicksearchbox.xiaomi.HotWordsProvider").apply {
            findMethod("isHotWordBackgroundRandom", Context::class.java).hook { replace { result = false } }
            findMethod("setHotWordBackgroundRandom", java.lang.Boolean.TYPE).hook {
                before {
                    args[0] = false
                }
            }
        }
    }
}
