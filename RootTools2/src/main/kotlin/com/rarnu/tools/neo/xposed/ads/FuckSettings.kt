@file:Suppress("RECEIVER_NULLABILITY_MISMATCH_BASED_ON_JAVA_ANNOTATIONS")

package com.rarnu.tools.neo.xposed.ads

import android.app.Activity
import android.os.Bundle
import android.preference.PreferenceActivity
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.FrameLayout
import android.widget.LinearLayout
import android.widget.ScrollView
import com.rarnu.xfunc.*

/**
 * Created by rarnu on 12/6/16.
 */
object FuckSettings {
    fun fuckSettings(pkg: XposedPkg) {

        // make developer settings always visible
        pkg.findClass("com.android.settings.MiuiSettings").findMethod("onCreate", Bundle::class.java).hook {
            after {
                val a = thisObject as Activity
                a.getSharedPreferences("development", 0).edit().putBoolean("show", true).apply()
            }
        }
        val clsU = pkg.findClass("com.android.settings.device.u")
        if (clsU != null && clsU.superclass.name == "com.android.settings.SettingsPreferenceFragment") {
            pkg.findClass("com.android.settings.MiuiSettings").findMethod("onHeaderClick", PreferenceActivity.Header::class.java, Integer.TYPE).hook {
                before {
                    val h = args[0] as PreferenceActivity.Header?
                    if (h?.fragment == "com.android.settings.device.MiuiDeviceDetailSettings") {
                        h.fragment = "com.android.settings.device.u"
                    }
                }
            }
        }

        val clsDetailSettings = pkg.findClass("com.android.settings.device.MiuiDeviceDetailSettings")
        if (clsDetailSettings != null) {
            pkg.findClass("com.android.settings.device.MiuiDeviceDetailSettings").findMethod("onInflateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java).hook {
                after {
                    val ll = result as LinearLayout
                    val sv = ll.getChildAt(0) as ScrollView
                    val li = sv.getChildAt(0) as LinearLayout
                    if (li.childCount == 5) {
                        val lay2 = li.getChildAt(1) as LinearLayout
                        lay2.getChildAt(1).visibility = View.GONE
                        lay2.getChildAt(2).visibility = View.GONE
                        lay2.getChildAt(7).visibility = View.GONE
                        lay2.getChildAt(8).visibility = View.GONE
                        lay2.getChildAt(9).visibility = View.GONE

                        (li.getChildAt(2) as FrameLayout).visibility = View.GONE
                        (li.getChildAt(3) as View).visibility = View.GONE
                        (li.getChildAt(4) as LinearLayout).visibility = View.GONE
                    }
                }
            }
        }

    }
}