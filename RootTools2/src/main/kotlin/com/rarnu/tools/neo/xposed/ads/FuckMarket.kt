@file:Suppress("DEPRECATION", "Duplicates")

package com.rarnu.tools.neo.xposed.ads

import android.content.Loader
import android.view.View
import com.rarnu.xfunc.*
import de.robv.android.xposed.XposedHelpers
import java.lang.reflect.Field
import java.util.*

/**
 * Created by rarnu on 12/6/16.
 */
object FuckMarket {

    fun fuckMarket(pkg: XposedPkg) {

        // 3.x
        val clsCg = pkg.findClass("com.xiaomi.market.data.cg")
        if (clsCg != null) {
            pkg.findClass("com.xiaomi.market.ui.UpdateAppsFragmentPhone").findMethod("a", Loader::class.java, clsCg).hook {
                before {
                    val loader = args[0] as Loader<*>
                    if (loader.id == 1) {
                        try { (XposedHelpers.getObjectField(args[1], "pL") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                    }
                }
            }
            pkg.findClass("com.xiaomi.market.ui.UpdateHistoryFragmentPhone").findMethod("a", Loader::class.java, clsCg).hook {
                before {
                    val loader = args[0] as Loader<*>
                    when (loader.id) {
                        1 -> try { (XposedHelpers.getObjectField(args[1], "pT") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        2 -> try { (XposedHelpers.getObjectField(args[1], "pL") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                    }
                }
            }
        }

        // 6.x 240, 250
        val clsCy = pkg.findClass("com.xiaomi.market.data.cy")
        if (clsCy != null) {
            pkg.findClass("com.xiaomi.market.ui.UpdateHistoryFragmentPhone").findMethod("a", Loader::class.java, clsCy).hook {
                before {
                    val loader = args[0] as Loader<*>
                    when (loader.id) {
                        1 -> {
                            try { (XposedHelpers.getObjectField(args[1], "asN") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            try { (XposedHelpers.getObjectField(args[1], "qD") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            try { (XposedHelpers.getObjectField(args[1], "qH") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        }
                        2 -> {
                            try { (XposedHelpers.getObjectField(args[1], "asE") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            try { (XposedHelpers.getObjectField(args[1], "qv") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            try { (XposedHelpers.getObjectField(args[1], "qz") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        }
                    }
                }
            }

            pkg.findClass("com.xiaomi.market.ui.UpdateAppsFragmentPhone").findMethod("a", Loader::class.java, clsCy).hook {
                before {
                    val loader = args[0] as Loader<*>
                    if (loader.id == 1) {
                        try { (XposedHelpers.getObjectField(args[1], "asE") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        try { (XposedHelpers.getObjectField(args[1], "qv") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        try { (XposedHelpers.getObjectField(args[1], "qz") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                    }
                }
            }
        }

        val clsQItem = pkg.findClass("com.xiaomi.market.ui.UpdateAppsAdapterPhone\$Item")
        if (clsQItem != null) {
            pkg.findClass("com.xiaomi.market.widget.q").findMethod("w", ArrayList::class.java).hook {
                before {
                    var fItemType: Field? = null
                    // 6.x 240
                    try {
                        fItemType = clsQItem.getDeclaredField("aTC")
                    } catch (e: Exception) {

                    }
                    if (fItemType == null) {
                        // 6.x 250
                        try {
                            fItemType = clsQItem.getDeclaredField("aVA")
                        } catch (e: Exception) {

                        }
                    }
                    if (fItemType != null) {
                        val list = args[0] as MutableList<*>?
                        val newList = mutableListOf<Any?>()
                        if (list != null) {
                            for (item in list) {
                                val typ = fItemType.get(item) as Enum<*>?
                                if (typ != null) {
                                    if (typ.toString() != "RECOMMEND_APP") {
                                        newList.add(item)
                                    }
                                }
                            }
                        }
                        args[0] = newList
                    }
                }
            }
            pkg.findClass("com.xiaomi.market.widget.q").findMethod("x", ArrayList::class.java).hook {
                before {
                    val fItemType = clsQItem.getDeclaredField("KV")
                    val list = args[0] as MutableList<*>?
                    val newList = mutableListOf<Any?>()
                    if (list != null) {
                        for (item in list) {
                            val typ = fItemType.get(item) as Enum<*>?
                            if (typ != null) {
                                if (typ.toString() != "RECOMMEND_APP") {
                                    newList.add(item)
                                }
                            }
                        }
                    }
                    args[0] = newList
                }
            }
        }

        // 6.x 250
        val clsCz = pkg.findClass("com.xiaomi.market.data.cz")
        if (clsCz != null) {
            pkg.findClass("com.xiaomi.market.ui.UpdateHistoryFragmentPhone").findMethod("a", Loader::class.java, clsCz).hook {
                before {
                    val loader = args[0] as Loader<*>
                    when (loader.id) {
                        1 -> {
                            // 3.x
                            try { (XposedHelpers.getObjectField(args[1], "atY") as MutableList<*>?)?.clear() } catch (e: Throwable) { }
                            // 6.x 250
                            try { (XposedHelpers.getObjectField(args[1], "aua") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (2)
                            try { (XposedHelpers.getObjectField(args[1], "atV") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (3)
                            try { (XposedHelpers.getObjectField(args[1], "auk") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (4)
                            try { (XposedHelpers.getObjectField(args[1], "auf") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (5)
                            try { (XposedHelpers.getObjectField(args[1], "aum") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250(6)
                            try { (XposedHelpers.getObjectField(args[1], "auo") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        }
                        2 -> {
                            // 3.x
                            try { (XposedHelpers.getObjectField(args[1], "atP") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250
                            try { (XposedHelpers.getObjectField(args[1], "atR") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (2)
                            try { (XposedHelpers.getObjectField(args[1], "atM") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (3)
                            try { (XposedHelpers.getObjectField(args[1], "aub") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (4)
                            try { (XposedHelpers.getObjectField(args[1], "atW") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (5)
                            try { (XposedHelpers.getObjectField(args[1], "aud") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250(6)
                            try { (XposedHelpers.getObjectField(args[1], "auf") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        }
                    }
                }
            }

            pkg.findClass("com.xiaomi.market.ui.UpdateAppsFragmentPhone").findMethod("a", Loader::class.java, clsCz).hook {
                before {
                    val loader = args[0] as Loader<*>
                    if (loader.id == 1) {
                        // 3.x
                        try { (XposedHelpers.getObjectField(args[1], "atP") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250
                        try { (XposedHelpers.getObjectField(args[1], "atR") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250 (2)
                        try { (XposedHelpers.getObjectField(args[1], "atM") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250 (3)
                        try { (XposedHelpers.getObjectField(args[1], "aub") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250 (4)
                        try { (XposedHelpers.getObjectField(args[1], "atW") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250 (5)
                        try { (XposedHelpers.getObjectField(args[1], "aud") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250(6)
                        try { (XposedHelpers.getObjectField(args[1], "auf") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                    }
                }
            }
        }

        // recommend view
        val clsRefInfo = pkg.findClass("com.xiaomi.market.model.RefInfo")

        // 3.x recommend
        val clsAL = pkg.findClass("com.xiaomi.market.model.al")
        if (clsAL != null && clsRefInfo != null) {
            pkg.findClass("com.xiaomi.market.ui.RelatedAppRecommendView").findMethod("a", clsAL, java.lang.Boolean.TYPE, List::class.java, clsRefInfo).hook {
                replace {
                    (thisObject as View?)?.visibility = View.GONE
                    result = null
                }
            }
        }

        // 6.x
        val clsAM = pkg.findClass("com.xiaomi.market.model.am")
        if (clsAM != null && clsRefInfo != null) {
            pkg.findClass("com.xiaomi.market.ui.RelatedAppRecommendView").findMethod("a", clsAM, java.lang.Boolean.TYPE, List::class.java, clsRefInfo).hook {
                replace {
                    (thisObject as View?)?.apply {
                        layoutParams?.height = 0
                        visibility = View.GONE
                    }
                    result = null
                }
            }
        }

        // 6.x.23.250
        val clsAN = pkg.findClass("com.xiaomi.market.model.an")
        if (clsAN != null && clsRefInfo != null) {
            pkg.findClass("com.xiaomi.market.ui.RelatedAppRecommendView").findMethod("a", clsAN, java.lang.Boolean.TYPE, List::class.java, clsRefInfo).hook {
                replace {
                    (thisObject as View?)?.visibility = View.GONE
                    result = null
                }
            }
        }

        // 7.1.6.260
        pkg.findClass("com.xiaomi.market.ui.DownloadListFragment").findMethod("a", Loader::class.java, clsCy).hook {
            before {
                val loader = args[0] as Loader<*>
                if (loader.id == 1) {
                    try { (XposedHelpers.getObjectField(args[1], "qz") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                }
            }
        }
    }
}