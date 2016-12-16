package com.rarnu.tools.neo.xposed.ads

import android.content.Loader
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.callbacks.XC_LoadPackage
import java.lang.reflect.Field
import java.util.*

/**
 * Created by rarnu on 12/6/16.
 */
object FuckMarket {

    fun fuckMarket(loadPackageParam: XC_LoadPackage.LoadPackageParam) {

        val clsCg = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.data.cg")
        if (clsCg != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateAppsFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCg, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    if (loader.id == 1) {
                        val list = param.args[1]
                        val fPL = list.javaClass.getDeclaredField("pL")
                        val l = fPL.get(list) as MutableList<*>?
                        l?.clear()
                    }
                }
            })
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateHistoryFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCg, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    val list = param.args[1]
                    when (loader.id) {
                        1 -> {
                            val fPT = list.javaClass.getDeclaredField("pT")
                            val l = fPT.get(list) as MutableList<*>?
                            l?.clear()
                        }
                        2 -> {
                            val fPL = list.javaClass.getDeclaredField("pL")
                            val l = fPL.get(list) as MutableList<*>?
                            l?.clear()
                        }
                    }
                }
            })
        }

        // 6.x / 240
        val clsCy = XpUtils.findClass(loadPackageParam.classLoader, "com.xiaomi.market.data.cy")
        if (clsCy != null) {
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateHistoryFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCy, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    val list = param.args[1]
                    when (loader.id) {
                        1 -> {
                            val fASN = list.javaClass.getDeclaredField("asN")
                            val l = fASN.get(list) as MutableList<*>?
                            l?.clear()
                        }
                        2 -> {
                            val fASE = list.javaClass.getDeclaredField("asE")
                            val l = fASE.get(list) as MutableList<*>?
                            l?.clear()
                        }
                    }
                }
            })
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateAppsFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCy, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    if (loader.id == 1) {
                        val list = param.args[1]
                        val fASE = list.javaClass.getDeclaredField("asE")
                        val l = fASE.get(list) as MutableList<*>?
                        l?.clear()
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
                    val list = param.args[1]
                    when (loader.id) {
                        1 -> {
                            val fATY = list.javaClass.getDeclaredField("atY")
                            val l = fATY.get(list) as MutableList<*>?
                            l?.clear()
                        }
                        2 -> {
                            val fATP = list.javaClass.getDeclaredField("atP")
                            val l = fATP.get(list) as MutableList<*>?
                            l?.clear()
                        }
                    }
                }
            })
            XpUtils.findAndHookMethod("com.xiaomi.market.ui.UpdateAppsFragmentPhone", loadPackageParam.classLoader, "a", Loader::class.java, clsCz, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val loader = param.args[0] as Loader<*>
                    if (loader.id == 1) {
                        val list = param.args[1]
                        try {
                            val fATP = list.javaClass.getDeclaredField("atP")
                            val l = fATP.get(list) as MutableList<*>?
                            l?.clear()
                        } catch (e: Exception) {

                        }
                        try {
                            val fATR = list.javaClass.getDeclaredField("atR")
                            val l = fATR.get(list) as MutableList<*>?
                            l?.clear()
                        } catch (e: Exception) {

                        }
                    }
                }
            })
        }

    }
}