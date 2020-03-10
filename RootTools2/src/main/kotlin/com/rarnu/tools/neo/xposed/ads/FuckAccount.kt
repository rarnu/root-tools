package com.rarnu.tools.neo.xposed.ads

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.rarnu.xfunc.*

/**
 * Created by rarnu on 1/3/17.
 */
object FuckAccount {

    fun fuckAccount(pkg: XposedPkg) {
        pkg.findClass("com.xiaomi.account.ui.n").findMethod("onCreateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java).hook {
            after {
                val ojT = thisObject.javaClass.findField("jT").getStatic()
                val oBannerGroup = ojT.javaClass.findField("mBannerGroup").get(ojT) as View?
                oBannerGroup?.visibility = View.GONE
            }
        }
        pkg.findClass("com.xiaomi.account.ui.o").findMethod("onCreateView", LayoutInflater::class.java, ViewGroup::class.java, Bundle::class.java).hook {
            after {
                val oJU = thisObject.javaClass.findField("jU").getStatic()
                val oBannerGroup = oJU.javaClass.findField("mBannerGroup").get(oJU) as View?
                oBannerGroup?.visibility = View.GONE
            }
        }
    }

    fun fuckVip(pkg: XposedPkg) {
        val clsVisibleItems = pkg.findClass("com.xiaomi.vip.ui.tasklist.TaskItemsDataPreprocessor\$VisibleItems")
        if (clsVisibleItems != null) {
            pkg.findClass("com.xiaomi.vip.ui.tasklist.TaskItemsDataPreprocessor").findMethod("addBanners", clsVisibleItems).hook { replace { result = null } }
        }
    }
}