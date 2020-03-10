@file:Suppress("RECEIVER_NULLABILITY_MISMATCH_BASED_ON_JAVA_ANNOTATIONS", "Duplicates")

package com.rarnu.tools.neo.xposed.ads

import com.rarnu.xfunc.*
import java.lang.reflect.Field

/**
 * Created by rarnu on 11/18/16.
 */
object FuckBrowser {

    fun fuckBrowser(pkg: XposedPkg) {
        val clsA = pkg.findClass("com.a.a.d.a")
        if (clsA != null) {
            pkg.findClass("com.android.browser.suggestion.af").apply {
                findMethod("a", clsA, String::class.java).hook {
                    after {
                        val o = thisObject
                        val fJ: Field?
                        fJ = if (o.javaClass.name != "com.android.browser.suggestion.af") {
                            o.javaClass.superclass.getDeclaredField("j")
                        } else {
                            o.javaClass.getDeclaredField("j")
                        }
                        fJ.isAccessible = true
                        val oJ = fJ.get(o)
                        if (oJ != null) {
                            if (oJ.javaClass.name == "com.android.browser.suggestion.SuggestItem\$AdsInfo") {
                                fJ.set(o, null)
                            }
                        }
                    }
                }
                findMethod("a", clsA).hook {
                    after {
                        val o = thisObject
                        val fJ: Field?
                        fJ = if (o.javaClass.name != "com.android.browser.suggestion.af") {
                            o.javaClass.superclass.getDeclaredField("j")
                        } else {
                            o.javaClass.getDeclaredField("j")
                        }
                        fJ.isAccessible = true
                        val oJ = fJ.get(o)
                        if (oJ != null) {
                            if (oJ.javaClass.name == "com.android.browser.suggestion.SuggestItem\$AdsInfo") {
                                fJ.set(o, null)
                            }
                        }
                    }
                }
            }
        }


        // 8.2.15
        pkg.findClass("com.android.browser.jx").findMethod("ak").hook { replace { result = true } }
        pkg.findClass("com.android.browser.suggestion.al").findMethod("e").hook { replace { result = 0 } }

        // 8.4.4
        pkg.findClass("com.android.browser.homepage.bk").findMethod("e").hook { replace { result = 0 } }
        pkg.findClass("com.android.browser.jv").apply {
            findMethod("ak").hook { replace { result = true } }
            findMethod("K").hook { replace { result = false } }
            findMethod("k").hook { replace { result = false } }
            findMethod("am").hook { replace { result = 1 } }
            findMethod("an").hook { replace { result = 1 } }
        }
        pkg.findClass("com.android.browser.suggestion.al").findMethod("f").hook { replace { result = 0 } }
        val clsSuggestItem = pkg.findClass("com.android.browser.suggestion.SuggestItem")
        if (clsSuggestItem != null) {
            pkg.findClass("com.android.browser.suggestion.ap").findMethod("a", clsSuggestItem).hook { replace { result = null } }
        }

        val clsAi = pkg.findClass("com.android.browser.suggestion.ai")
        if (clsSuggestItem != null && clsAi != null) {
            if (clsAi.superclass.name == "android.widget.BaseAdapter") {
                pkg.findClass("com.android.browser.suggestion.ai").findMethod("a", List::class.java, String::class.java).hook {
                    before {
                        val list = args[0] as MutableList<*>?
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
                        args[0] = list
                    }
                }
            }
        }
        pkg.findClass("miui.browser.a").findMethod("a", String::class.java).hook {
            before {
                val str = args[0] as String?
                if (str == "v6_apk_download_guide_to_market") {
                    result = false
                }
            }
            after {
                val str = args[0] as String?
                if (str == "v6_apk_download_guide_to_market") {
                    result = false
                }
            }
        }
        pkg.findClass("com.android.browser.or").findMethod("a", String::class.java, String::class.java, String::class.java).hook { replace { result = null } }

        // 8.5.3
        pkg.findClass("com.android.browser.view.au").findMethod("setItemCount", Integer.TYPE).hook {
            before {
                var i = args[0] as Int
                if (i == 2) {
                    i = 1
                }
                args[0] = i
            }
        }
        pkg.findClass("com.android.browser.os").findMethod("a", String::class.java, String::class.java, String::class.java).hook { replace { result = null } }

        val clsBrowserActivity = pkg.findClass("com.android.browser.BrowserActivity")
        val clsTu = pkg.findClass("com.android.browser.tu")
        val clsOs = pkg.findClass("com.android.browser.os")
        if (clsBrowserActivity != null && clsTu != null && clsOs != null) {
            pkg.findClass("com.android.browser.kc").findConstructor(clsBrowserActivity, clsTu, clsOs).hook {
                before {
                    thisObject.javaClass.findField("c").setStatic(true)
                }
            }
        }

        // 8.5.6
        val clsTs = pkg.findClass("com.android.browser.ts")
        if (clsBrowserActivity != null && clsTs != null && clsOs != null) {
            pkg.findClass("com.android.browser.kc").findConstructor(clsBrowserActivity, clsTs, clsOs).hook {
                before {
                    thisObject.javaClass.findField("c").setStatic(true)
                }
            }
        }

        // 8.5.10
        pkg.findClass("com.android.browser.ov").findMethod("a", String::class.java, String::class.java, String::class.java).hook { replace { result = null } }

        val clsTx = pkg.findClass("com.android.browser.tx")
        val clsOv = pkg.findClass("com.android.browser.ov")
        if (clsBrowserActivity != null && clsTx != null && clsOv != null) {
            pkg.findClass("com.android.browser.kf").findConstructor(clsBrowserActivity, clsTx, clsOv).hook {
                before {
                    thisObject.javaClass.findField("c").setStatic(true)
                }
            }
        }

        // 8.5.11
        pkg.findClass("com.android.browser.pa").findMethod("a", String::class.java, String::class.java, String::class.java).hook { replace { result = null } }
        val clsUd = pkg.findClass("com.android.browser.ud")
        val clsPa = pkg.findClass("com.android.browser.pa")
        if (clsBrowserActivity != null && clsUd != null && clsPa != null) {
            pkg.findClass("com.android.browser.kf").findConstructor(clsBrowserActivity, clsUd, clsPa).hook {
                before {
                    thisObject.javaClass.findField("c").setStatic(true)
                }
            }
        }
        pkg.findClass("com.android.browser.view.at").findMethod("setItemCount", Integer.TYPE).hook {
            before {
                var i = args[0] as Int
                if (i == 2) {
                    i = 1
                }
                args[0] = i
            }
        }
    }
}
