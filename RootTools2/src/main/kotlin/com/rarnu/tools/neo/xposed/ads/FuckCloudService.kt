package com.rarnu.tools.neo.xposed.ads

import com.rarnu.xfunc.*

/**
 * Created by rarnu on 12/20/16.
 */
object FuckCloudService {

    fun fuckCloudService(pkg: XposedPkg) {
        // old
        pkg.findClass("com.miui.cloudservice.ui.x").apply {
            // old
            findMethod("hd").hook { replace { result = true } }
            // new
            findMethod("hg").hook { replace { result = true } }
        }
    }

}