package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.os.Bundle
import com.rarnu.xfunc.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckAdSolution {

    fun fuckAdSolution(pkg: XposedPkg) {
        pkg.findClass("com.xiaomi.ad.internal.common.b.c").findMethod("a", String::class.java, Context::class.java).hook { replace { result = false } }
        pkg.findClass("com.xiaomi.ad.internal.common.b.c").findMethod("j", Context::class.java, String::class.java).hook { replace { result = false } }
        pkg.findClass("com.xiaomi.ad.internal.common.b.d").findMethod("m", Context::class.java, String::class.java).hook { replace { result = null } }
        pkg.findClass("com.miui.systemAdSolution.landingPage.h").findMethod("a", String::class.java, String::class.java, Integer.TYPE).hook { replace { result = null } }

        val clsB = pkg.findClass("com.xiaomi.ad.internal.pojo.b")
        if (clsB != null) {
            pkg.findClass("com.xiaomi.ad.internal.server.h").apply {
                findMethod("a", clsB, Bundle::class.java).hook { replace { result = null } }
                findMethod("b", clsB, Bundle::class.java).hook { replace { result = null } }
            }
        }
        pkg.findClass("com.xiaomi.ad.internal.ui.SplashAdView").findMethod("onAction", String::class.java).hook { replace { result = null } }
    }
}
