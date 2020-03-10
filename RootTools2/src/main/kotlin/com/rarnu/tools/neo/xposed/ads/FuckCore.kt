package com.rarnu.tools.neo.xposed.ads

import com.rarnu.xfunc.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckCore {

    fun fuckCore(pkg: XposedPkg) {
        pkg.findClass("miui.os.SystemProperties").findMethod("get", String::class.java, String::class.java).hook {
            before {
                if (args[0].toString() == "ro.product.mod_device") {
                    result = "gemini_global"
                }
            }
            after {
                if (args[0].toString() == "ro.product.mod_device") {
                    result = "gemini_global"
                }
            }
        }
        pkg.findClass("miui.os.SystemProperties.Build").findField("IS_CM_CUSTOMIZATION_TEST").setStatic(true)
        pkg.findClass("com.miui.internal.util").findField("IS_INTERNATIONAL_BUILD").setStatic(true)
    }

}
