package com.rarnu.tools.neo.xposed

import android.annotation.SuppressLint
import android.os.Bundle
import android.os.Message
import android.widget.Button
import android.widget.TextView
import com.rarnu.xfunc.*
import de.robv.android.xposed.*

import de.robv.android.xposed.callbacks.XC_LayoutInflated


@SuppressLint("StaticFieldLeak")
private var warningText: TextView? = null
@SuppressLint("StaticFieldLeak")
private var accept: Button? = null

class MIUIRoot25Resources: XposedResource() {

    override fun hook(res: XposedRes) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (res.packageName == "com.miui.securitycenter") {
            if (prefs.getBoolean(XpStatus.KEY_ROOTCRACK, false)) {
                res.res.hookLayout("com.miui.securitycenter", "layout", "pm_activity_root_apply", object : XC_LayoutInflated() {
                    override fun handleLayoutInflated(paramAnonymousLayoutInflatedParam: LayoutInflatedParam) {
                        accept = paramAnonymousLayoutInflatedParam.view.findViewById(paramAnonymousLayoutInflatedParam.res.getIdentifier("accept", "id", "com.miui.securitycenter"))
                        warningText = paramAnonymousLayoutInflatedParam.view.findViewById(paramAnonymousLayoutInflatedParam.res.getIdentifier("warning_info", "id", "com.miui.securitycenter"))
                        warningText?.setLines(6)
                    }
                })
            }
        }
    }
}

class MIUIRoot25Pakage : XposedPackage() {
    override fun hook(pkg: XposedPkg) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (pkg.packageName == "com.miui.securitycenter") {
            if (prefs.getBoolean(XpStatus.KEY_ROOTCRACK, false)) {
                pkg.findClass("com.miui.permcenter.root.RootApplyActivity").findMethod("onCreate", Bundle::class.java).hook {
                    after {
                        result = null
                        if (accept == null) {
                            return@after
                        }
                        var i = 0
                        while (i < 5) {
                            accept?.performClick()
                            i += 1
                        }
                    }
                }
                pkg.findClass("com.miui.permcenter.root.c").findMethod("handleMessage", Message::class.java).hook { replace { result = null } }
                pkg.findClass("com.miui.permcenter.root.a").findMethod("handleMessage", Message::class.java).hook { replace { result = null } }
            }
        }
    }
}
