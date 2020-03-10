@file:Suppress("Duplicates")

package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.content.Intent
import android.preference.PreferenceManager
import com.rarnu.xfunc.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckCleanMaster {

    fun fuckCleanMaster(pkg: XposedPkg) {
        pkg.findClass("com.miui.optimizecenter.result.DataModel").findMethod("post", Map::class.java).hook { replace { result = "" } }
        pkg.findClass("com.miui.optimizecenter.config.MiStat").findMethod("getChannel").hook { replace { result = "international" } }

        val clsAdImageView = pkg.findClass("com.miui.optimizecenter.widget.AdImageView")
        val clsAdvertisement = pkg.findClass("com.miui.optimizecenter.result.Advertisement")
        if (clsAdImageView != null && clsAdvertisement != null) {
            pkg.findClass("com.miui.optimizecenter.result.CleanResultActivity").apply {
                findMethod("startAdCountdown", clsAdImageView, clsAdvertisement).hook { replace { result = null } }
                findMethod("addAdvertisementEvent", String::class.java, clsAdvertisement).hook { replace { result = null } }
            }
        }
        pkg.findClass("com.miui.optimizecenter.Application").findMethod("attachBaseContext", Context::class.java).hook {
            after {
                val pref = PreferenceManager.getDefaultSharedPreferences(thisObject as Context)
                pref.edit().putBoolean("key_information_setting_close", false).apply()
            }
        }
    }

    fun fuckSecurityCenter(pkg: XposedPkg) {
        pkg.findClass("com.miui.securitycenter.Application").findMethod("attachBaseContext", Context::class.java).hook {
            after {
                val pref = PreferenceManager.getDefaultSharedPreferences(thisObject as Context)
                pref.edit().putBoolean("key_information_setting_close", false).apply()
            }
        }
        pkg.findClass("com.miui.securitycenter.service.i").apply {
            findMethod("oI").hook { replace { result = null } }
            findMethod("oy").hook { replace { result = null } }
            findMethod("a", String::class.java, Intent::class.java).hook { replace { result = null } }
        }
    }

    fun fuckResource(res: XposedRes) {
        res.res.setReplacement(res.packageName, "string", "no_network", "")
    }

}
