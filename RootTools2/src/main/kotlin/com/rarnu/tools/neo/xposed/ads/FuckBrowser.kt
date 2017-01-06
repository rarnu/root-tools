package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage
import java.lang.reflect.Field

/**
 * Created by rarnu on 11/18/16.
 */
object FuckBrowser {

    fun fuckBrowser(loadPackageParam: XC_LoadPackage.LoadPackageParam) {

        //
        val clsA = XpUtils.findClass(loadPackageParam.classLoader, "com.a.a.d.a")
        if (clsA != null) {
            XpUtils.findAndHookMethod("com.android.browser.suggestion.af", loadPackageParam.classLoader, "a", clsA, String::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    val o = param.thisObject
                    val fJ: Field?
                    if (o.javaClass.name != "com.android.browser.suggestion.af") {
                        fJ = o.javaClass.superclass.getDeclaredField("j")
                    } else {
                        fJ = o.javaClass.getDeclaredField("j")
                    }
                    fJ.isAccessible = true
                    val oJ = fJ.get(o)
                    if (oJ != null) {
                        if (oJ.javaClass.name == "com.android.browser.suggestion.SuggestItem\$AdsInfo") {
                            fJ.set(o, null)
                        }
                    }
                }
            })

            XpUtils.findAndHookMethod("com.android.browser.suggestion.af", loadPackageParam.classLoader, "a", clsA, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(param: MethodHookParam) {
                    val o = param.thisObject
                    val fJ: Field?
                    if (o.javaClass.name != "com.android.browser.suggestion.af") {
                        fJ = o.javaClass.superclass.getDeclaredField("j")
                    } else {
                        fJ = o.javaClass.getDeclaredField("j")
                    }
                    fJ.isAccessible = true
                    val oJ = fJ.get(o)
                    if (oJ != null) {
                        if (oJ.javaClass.name == "com.android.browser.suggestion.SuggestItem\$AdsInfo") {
                            fJ.set(o, null)
                        }
                    }
                }
            })
        }


        // 8.2.15
        XpUtils.findAndHookMethod("com.android.browser.jx", loadPackageParam.classLoader, "ak", XC_MethodReplacement.returnConstant(true))
        XpUtils.findAndHookMethod("com.android.browser.suggestion.al", loadPackageParam.classLoader, "e", XC_MethodReplacement.returnConstant(0))

        // 8.4.4
        XpUtils.findAndHookMethod("com.android.browser.homepage.bk", loadPackageParam.classLoader, "e", XC_MethodReplacement.returnConstant(0))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "ak", XC_MethodReplacement.returnConstant(true))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "K", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "k", XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "am", XC_MethodReplacement.returnConstant(1))
        XpUtils.findAndHookMethod("com.android.browser.jv", loadPackageParam.classLoader, "an", XC_MethodReplacement.returnConstant(1))

        XpUtils.findAndHookMethod("com.android.browser.suggestion.al", loadPackageParam.classLoader, "f", XC_MethodReplacement.returnConstant(0))
        val clsSuggestItem = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.suggestion.SuggestItem")
        if (clsSuggestItem != null) {
            XpUtils.findAndHookMethod("com.android.browser.suggestion.ap", loadPackageParam.classLoader, "a", clsSuggestItem, XC_MethodReplacement.returnConstant(null))
        }

        val clsAi = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.suggestion.ai")
        if (clsSuggestItem != null && clsAi != null) {
            if (clsAi.superclass.name == "android.widget.BaseAdapter") {
                XpUtils.findAndHookMethod("com.android.browser.suggestion.ai", loadPackageParam.classLoader, "a", List::class.java, String::class.java, object : XC_MethodHook() {
                    @Throws(Throwable::class)
                    override fun beforeHookedMethod(param: MethodHookParam) {
                        val list = param.args[0] as MutableList<*>?
                        val fType = clsSuggestItem.getDeclaredField("type")
                        fType.isAccessible = true
                        if (list != null) {
                            var idx = list.size - 1
                            while (idx >= 0) {
                                val item = list[idx]    // SuggestItem
                                val typ = fType.get(item)
                                if (typ == "app") {
                                    list.removeAt(idx)
                                }
                                idx--
                            }
                        }
                        param.args[0] = list
                    }
                })
            }
        }

        XpUtils.findAndHookMethod("miui.browser.a", loadPackageParam.classLoader, "a", String::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                val str = param.args[0] as String?
                if (str == "v6_apk_download_guide_to_market") {
                    param.result = false
                }
            }

            @Throws(Throwable::class)
            override fun afterHookedMethod(param: MethodHookParam) {
                val str = param.args[0] as String?
                if (str == "v6_apk_download_guide_to_market") {
                    param.result = false
                }
            }
        })
        XpUtils.findAndHookMethod("com.android.browser.or", loadPackageParam.classLoader, "a", String::class.java, String::class.java, String::class.java, XC_MethodReplacement.returnConstant(null))

        // 8.5.3
        XpUtils.findAndHookMethod("com.android.browser.view.au", loadPackageParam.classLoader, "setItemCount", Integer.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                var i = param.args[0] as Int
                if (i == 2) {
                    i = 1
                }
                param.args[0] = i
            }
        })
        XpUtils.findAndHookMethod("com.android.browser.os", loadPackageParam.classLoader, "a", String::class.java, String::class.java, String::class.java, XC_MethodReplacement.returnConstant(null))

        val clsBrowserActivity = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.BrowserActivity")
        val clsTu = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.tu")
        val clsOs = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.os")
        if (clsBrowserActivity != null && clsTu != null && clsOs != null) {
            XpUtils.findAndHookConstructor("com.android.browser.kc", loadPackageParam.classLoader, clsBrowserActivity, clsTu, clsOs, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val o = param.thisObject
                    val fC = o.javaClass.getDeclaredField("c")
                    fC.isAccessible = true
                    fC.set(null, true)
                }
            })
        }

        // 8.5.6
        val clsTs = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.ts")
        if (clsBrowserActivity != null && clsTs != null && clsOs != null) {
            XpUtils.findAndHookConstructor("com.android.browser.kc", loadPackageParam.classLoader, clsBrowserActivity, clsTs, clsOs, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val o = param.thisObject
                    val fC = o.javaClass.getDeclaredField("c")
                    fC.isAccessible = true
                    fC.set(null, true)
                }
            })
        }

        // 8.5.10
        XpUtils.findAndHookMethod("com.android.browser.ov", loadPackageParam.classLoader, "a", String::class.java, String::class.java, String::class.java, XC_MethodReplacement.returnConstant(null))
        val clsTx = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.tx")
        val clsOv = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.ov")
        if (clsBrowserActivity != null && clsTx != null && clsOv != null) {
            XpUtils.findAndHookConstructor("com.android.browser.kf", loadPackageParam.classLoader, clsBrowserActivity, clsTx, clsOv, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val o = param.thisObject
                    val fC = o.javaClass.getDeclaredField("c")
                    fC.isAccessible = true
                    fC.set(null, true)
                }
            })
        }

        // 8.5.11
        XpUtils.findAndHookMethod("com.android.browser.pa", loadPackageParam.classLoader, "a", String::class.java, String::class.java, String::class.java, XC_MethodReplacement.returnConstant(null))
        val clsUd = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.ud")
        val clsPa = XpUtils.findClass(loadPackageParam.classLoader, "com.android.browser.pa")
        if (clsBrowserActivity != null && clsUd != null && clsPa != null) {
            XpUtils.findAndHookConstructor("com.android.browser.kf", loadPackageParam.classLoader, clsBrowserActivity, clsUd, clsPa, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val o = param.thisObject
                    val fC = o.javaClass.getDeclaredField("c")
                    fC.isAccessible = true
                    fC.set(null, true)
                }
            })
        }
        XpUtils.findAndHookMethod("com.android.browser.view.at", loadPackageParam.classLoader, "setItemCount", Integer.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                var i = param.args[0] as Int
                if (i == 2) {
                    i = 1
                }
                param.args[0] = i
            }
        })

    }
}
