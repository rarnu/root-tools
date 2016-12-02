package com.rarnu.tools.neo.xposed

import android.content.Context
import android.content.Intent
import com.rarnu.tools.neo.utils.FileUtils
import de.robv.android.xposed.*
import de.robv.android.xposed.callbacks.XC_LoadPackage

import java.io.File

class MIUITheme : IXposedHookZygoteInit, IXposedHookLoadPackage {

    @Throws(Throwable::class)
    override fun initZygote(startupParam: IXposedHookZygoteInit.StartupParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (!prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
            return
        }
        MIUI_DRM()
    }

    @Throws(Throwable::class)
    override fun handleLoadPackage(lpparam: XC_LoadPackage.LoadPackageParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (lpparam.packageName == "miui.drm" || lpparam.packageName == "com.miui.system" || lpparam.packageName == "miui.system") {
            if (prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
                MIUI_DRM()
            }
            return
        }
        if (lpparam.packageName == "com.android.thememanager") {
            if (prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
                XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isTrialable", XC_MethodReplacement.returnConstant(false))
                XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isLegal", XC_MethodReplacement.returnConstant(true))
                XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isAuthorizedResource", XC_MethodReplacement.returnConstant(true))
                XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isPermanentRights", XC_MethodReplacement.returnConstant(true))
            }
            if (prefs.getBoolean(XpStatus.KEY_KEEP_MTZ, false)) {
                XpUtils.findAndHookMethod("com.android.thememanager.model.ResourceOnlineProperties", lpparam.classLoader, "setDownloadPath", String::class.java, object : XC_MethodHook() {
                    @Throws(Throwable::class)
                    override fun beforeHookedMethod(param: XC_MethodHook.MethodHookParam) {
                        val oriPath = param.args[0] as String
                        val newPath = oriPath + "1.mtz"
                        XposedBridge.log("setDownloadPath => " + newPath)
                        try {
                            FileUtils.copyFile(oriPath, newPath)
                            XposedBridge.log("setDownloadPath copy file => succ")
                            param.args[0] = newPath
                        } catch (e: Exception) {
                            XposedBridge.log("setDownloadPath error => " + e.toString())
                        }

                    }
                })
            }
        }
    }

    private fun MIUI_DRM() {
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", arrayOf(Context::class.java, File::class.java, File::class.java, XC_MethodReplacement.returnConstant(drmResultSUCCESS)))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", arrayOf(Context::class.java, String::class.java, File::class.java, XC_MethodReplacement.returnConstant(drmResultSUCCESS)))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", arrayOf(Context::class.java, String::class.java, "miui.drm.DrmManager\$RightObject", XC_MethodReplacement.returnConstant(drmResultSUCCESS)))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isPermanentRights", arrayOf(File::class.java, XC_MethodReplacement.returnConstant(true)))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", arrayOf("miui.drm.DrmManager\$RightObject", XC_MethodReplacement.returnConstant(true)))
        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "validateTheme", arrayOf<Any>(XC_MethodReplacement.returnConstant(true)))
        XpUtils.findAndHookMethod("miui.content.res.ThemeRuntimeManager.ThemeReceiver", "validateTheme", arrayOf<Any>(XC_MethodReplacement.returnConstant(true)))
        XpUtils.findAndHookMethod("miui.content.res.ThemeRuntimeManager", "restoreDefault", arrayOf<Any>(object : XC_MethodReplacement() {
            @Throws(Throwable::class)
            override fun replaceHookedMethod(param: XC_MethodHook.MethodHookParam): Any? {
                return null
            }
        }))
        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "restoreDefault", arrayOf<Any>(object : XC_MethodReplacement() {
            @Throws(Throwable::class)
            override fun replaceHookedMethod(param: XC_MethodHook.MethodHookParam): Any? {
                return null
            }
        }))
        XpUtils.findAndHookMethod("miui.content.res.ThemeRuntimeManager", "restoreDefault", arrayOf<Any>(object : XC_MethodReplacement() {
            @Throws(Throwable::class)
            override fun replaceHookedMethod(param: XC_MethodHook.MethodHookParam): Any? {
                return null
            }
        }))
        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "onReceive", arrayOf(Context::class.java, Intent::class.java, object : XC_MethodReplacement() {
            @Throws(Throwable::class)
            override fun replaceHookedMethod(param: XC_MethodHook.MethodHookParam): Any? {
                return null
            }
        }))
    }

    companion object {

        val drmResultSUCCESS: Any?
            get() {
                try {
                    val clsEnum = Class.forName("miui.drm.DrmManager\$DrmResult")

                    if (clsEnum != null) {
                        val mEnum = clsEnum.getDeclaredMethod("valueOf", String::class.java)
                        return mEnum.invoke(null, "DRM_SUCCESS")
                    }
                } catch (th: Throwable) {
                    XposedBridge.log(th.toString())

                }
                return null
            }
    }

}

