package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import android.view.View
import com.rarnu.xfunc.*
import de.robv.android.xposed.XposedHelpers
import java.util.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckWeather {

    fun fuckWeather(pkg: XposedPkg) {
        pkg.findClass("com.miui.weather2.tools.ToolUtils").apply {
            findMethod("checkCommericalStatue", Context::class.java).hook { replace { result = false } }
            findMethod("canRequestCommercialInfo", Context::class.java).hook { replace { result = false } }
        }

        pkg.findClass("com.miui.weather2.ActivityDailyForecastDetail").apply {
            findMethod("ep").hook { replace { result = null } }
            findMethod("eq").hook { replace { result = null } }
            findMethod("er").hook { replace { result = null } }
            findMethod("eo").hook { replace { result = null } }
        }
        pkg.findClass("com.miui.weather2.structures.DailyForecastAdData").apply {
            findMethod("isAdInfosExistence").hook { replace { result = false } }
            findMethod("isAdTitleExistence").hook { replace { result = false } }
            findMethod("isLandingPageUrlExistence").hook { replace { result = false } }
            findMethod("isUseSystemBrowserExistence").hook { replace { result = false } }
        }

        // 8.2.1
        pkg.findClass("com.miui.weather2.WeatherApplication").findMethod("attachBaseContext", Context::class.java).hook {
            after {
                val ctx = args[0] as Context?
                val pref = ctx?.getSharedPreferences("com.miui.weather2.information", 0)
                pref?.edit()?.putBoolean("agree_to_have_information", false)?.apply()
            }
        }

        // 8.2.3
        pkg.findClass("com.miui.weather2.view.n").findMethod("i", ArrayList::class.java).hook { replace { result = null } }

        pkg.findClass("com.miui.weather2.view.WeatherScrollView").findMethod("mu").hook {
            after {
                val v = XposedHelpers.getObjectField(thisObject, "LR") as View?
                v?.visibility = View.GONE
            }
        }
    }
}
