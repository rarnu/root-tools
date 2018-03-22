package com.rarnu.tools.neo.xposed

import android.os.Bundle
import android.os.Message
import android.widget.Button
import android.widget.TextView
import de.robv.android.xposed.*
import de.robv.android.xposed.callbacks.XC_InitPackageResources
import de.robv.android.xposed.callbacks.XC_LayoutInflated
import de.robv.android.xposed.callbacks.XC_LoadPackage

class MIUIRoot25 : IXposedHookInitPackageResources, IXposedHookLoadPackage {

    @Throws(Throwable::class)
    override fun handleLoadPackage(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (loadPackageParam.packageName == "com.miui.securitycenter") {
            if (prefs.getBoolean(XpStatus.KEY_ROOTCRACK, false)) {
                XpUtils.findAndHookMethod("com.miui.permcenter.root.RootApplyActivity", loadPackageParam.classLoader, "onCreate", Bundle::class.java, object : XC_MethodHook() {
                    @Throws(Throwable::class)
                    override fun afterHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam?) {
                        if (accept == null) {
                            return
                        }
                        var i = 0
                        while (i < 5) {
                            accept?.performClick()
                            i += 1
                        }
                    }
                })
                XpUtils.findAndHookMethod("com.miui.permcenter.root.c", loadPackageParam.classLoader, "handleMessage", Message::class.java, object : XC_MethodReplacement() {
                    @Throws(Throwable::class)
                    override fun replaceHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam): Any? {
                        return null
                    }
                })
                XpUtils.findAndHookMethod("com.miui.permcenter.root.a", loadPackageParam.classLoader, "handleMessage", Message::class.java, object : XC_MethodReplacement() {
                    @Throws(Throwable::class)
                    override fun replaceHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam): Any? {
                        return null
                    }
                })
            }
        }
    }


    @Throws(Throwable::class)
    override fun handleInitPackageResources(initPackageResourcesParam: XC_InitPackageResources.InitPackageResourcesParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (initPackageResourcesParam.packageName == "com.miui.securitycenter") {
            if (prefs.getBoolean(XpStatus.KEY_ROOTCRACK, false)) {
                initPackageResourcesParam.res.hookLayout("com.miui.securitycenter", "layout", "pm_activity_root_apply", object : XC_LayoutInflated() {
                    @Throws(Throwable::class)
                    override fun handleLayoutInflated(paramAnonymousLayoutInflatedParam: XC_LayoutInflated.LayoutInflatedParam) {
                        accept = paramAnonymousLayoutInflatedParam.view.findViewById(paramAnonymousLayoutInflatedParam.res.getIdentifier("accept", "id", "com.miui.securitycenter")) as Button
                        warningText = paramAnonymousLayoutInflatedParam.view.findViewById(paramAnonymousLayoutInflatedParam.res.getIdentifier("warning_info", "id", "com.miui.securitycenter")) as TextView
                        if (warningText != null) {
                            warningText?.setLines(6)
                        }
                    }
                })
            }
        }
    }

    companion object {
        internal var warningText: TextView? = null
        internal var accept: Button? = null
    }
}
