package com.rarnu.tools.neo.xposed.ads

import android.content.Loader
import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.XposedBridge
import de.robv.android.xposed.XposedHelpers
import de.robv.android.xposed.callbacks.XC_LoadPackage
import org.jetbrains.annotations.Mutable
import java.lang.reflect.Field
import java.util.*

/**
 * Created by rarnu on 12/6/16.
 */
object FuckMarket {

    fun fuckMarket(loadPackageParam: XC_LoadPackage.LoadPackageParam) {

        // 3.x
        val clsCg = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.data.cg")
        if (clsCg != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateAppsFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCg, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    if (loader.id == 1) {
                        try { (XposedHelpers.getObjectField(param.args[1], "pL") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                    }
                }
            })
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateHistoryFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCg, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    when (loader.id) {
                        1 -> try { (XposedHelpers.getObjectField(param.args[1], "pT") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        2 -> try { (XposedHelpers.getObjectField(param.args[1], "pL") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                    }
                }
            })
        }

        // 6.x 240, 250
        val clsCy = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.data.cy")
        if (clsCy != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateHistoryFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCy, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    when (loader.id) {
                        1 -> {
                            try { (XposedHelpers.getObjectField(param.args[1], "asN") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            try { (XposedHelpers.getObjectField(param.args[1], "qD") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        }
                        2 -> {
                            try { (XposedHelpers.getObjectField(param.args[1], "asE") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            try { (XposedHelpers.getObjectField(param.args[1], "qv") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        }
                    }
                }
            })
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateAppsFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCy, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    if (loader.id == 1) {
                        try { (XposedHelpers.getObjectField(param.args[1], "asE") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        try { (XposedHelpers.getObjectField(param.args[1], "qv") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                    }
                }
            })
        }

        val clsQItem = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.ui.UpdateAppsAdapterPhone\$Item")
        if (clsQItem != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.widget.q", loadPackageParam.classLoader, "w", ArrayList::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
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
                        val list = param.args[0] as MutableList<*>?
                        val newList = arrayListOf<Any?>()
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
                        param.args[0] = newList
                    }
                }
            })

            XpUtils.findAndHookMethod("com.xiaomi.market.widget.q", loadPackageParam.classLoader, "x", ArrayList::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    var fItemType = clsQItem.getDeclaredField("KV")
                    val list = param.args[0] as MutableList<*>?
                    val newList = arrayListOf<Any?>()
                    if (list != null) {
                        for (item in list) {
                            val typ = fItemType?.get(item) as Enum<*>?
                            if (typ != null) {
                                if (typ.toString() != "RECOMMEND_APP") {
                                    newList.add(item)
                                }
                            }
                        }
                    }
                    param.args[0] = newList
                }
            })
        }

        // 6.x 250
        val clsCz = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.data.cz")
        if (clsCz != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateHistoryFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCz, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    when (loader.id) {
                        1 -> {
                            // 3.x
                            try { (XposedHelpers.getObjectField(param.args[1], "atY") as MutableList<*>?)?.clear() } catch (e: Throwable) { }
                            // 6.x 250
                            try { (XposedHelpers.getObjectField(param.args[1], "aua") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (2)
                            try { (XposedHelpers.getObjectField(param.args[1], "atV") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (3)
                            try { (XposedHelpers.getObjectField(param.args[1], "auk") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (4)
                            try { (XposedHelpers.getObjectField(param.args[1], "auf") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (5)
                            try { (XposedHelpers.getObjectField(param.args[1], "aum") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250(6)
                            try { (XposedHelpers.getObjectField(param.args[1], "auo") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        }
                        2 -> {
                            // 3.x
                            try { (XposedHelpers.getObjectField(param.args[1], "atP") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250
                            try { (XposedHelpers.getObjectField(param.args[1], "atR") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (2)
                            try { (XposedHelpers.getObjectField(param.args[1], "atM") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (3)
                            try { (XposedHelpers.getObjectField(param.args[1], "aub") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (4)
                            try { (XposedHelpers.getObjectField(param.args[1], "atW") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250 (5)
                            try { (XposedHelpers.getObjectField(param.args[1], "aud") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                            // 6.x 250(6)
                            try { (XposedHelpers.getObjectField(param.args[1], "auf") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        }
                    }
                }
            })

            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateAppsFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCz, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    if (loader.id == 1) {
                        // 3.x
                        try { (XposedHelpers.getObjectField(param.args[1], "atP") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250
                        try { (XposedHelpers.getObjectField(param.args[1], "atR") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250 (2)
                        try { (XposedHelpers.getObjectField(param.args[1], "atM") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250 (3)
                        try { (XposedHelpers.getObjectField(param.args[1], "aub") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250 (4)
                        try { (XposedHelpers.getObjectField(param.args[1], "atW") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250 (5)
                        try { (XposedHelpers.getObjectField(param.args[1], "aud") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                        // 6.x 250(6)
                        try { (XposedHelpers.getObjectField(param.args[1], "auf") as MutableList<*>?)?.clear() } catch (t: Throwable) { }
                    }
                }
            })
        }

        // recommend view
        val clsRefInfo = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.model.RefInfo")

        // 3.x recommend
        val clsAL = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.model.al")
        if (clsAL != null && clsRefInfo != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.RelatedAppRecommendView", loadPackageParam.classLoader, "a", clsAL, java.lang.Boolean.TYPE, List::class.java, clsRefInfo, object : XC_MethodReplacement() {
                @Throws(Throwable::class)
                override fun replaceHookedMethod(param: MethodHookParam): Any? {
                    (param.thisObject as View?)?.visibility = View.GONE
                    return null
                }
            })
        }

        // 6.x
        val clsAM = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.model.am")
        if (clsAM != null && clsRefInfo != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.RelatedAppRecommendView", loadPackageParam.classLoader, "a", clsAM, java.lang.Boolean.TYPE, List::class.java, clsRefInfo, object : XC_MethodReplacement() {
                @Throws(Throwable::class)
                override fun replaceHookedMethod(param: MethodHookParam): Any? {
                    val v = param.thisObject as View?
                    v?.layoutParams?.height = 0
                    v?.visibility = View.GONE
                    return null
                }
            })
        }

        // 6.x.23.250
        val clsAN = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.model.an")
        if (clsAN != null && clsRefInfo != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.RelatedAppRecommendView", loadPackageParam.classLoader, "a", clsAN, java.lang.Boolean.TYPE, List::class.java, clsRefInfo, object : XC_MethodReplacement() {
                @Throws(Throwable::class)
                override fun replaceHookedMethod(param: MethodHookParam): Any? {
                    (param.thisObject as View?)?.visibility = View.GONE
                    return null
                }
            })
        }
    }
}