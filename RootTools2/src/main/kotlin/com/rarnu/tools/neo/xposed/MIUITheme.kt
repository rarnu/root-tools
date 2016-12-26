package com.rarnu.tools.neo.xposed

import android.content.Context
import android.content.Intent
import de.robv.android.xposed.*
import de.robv.android.xposed.callbacks.XC_LoadPackage
import java.io.File
import java.io.InputStream

class MIUITheme : IXposedHookZygoteInit, IXposedHookLoadPackage {

    @Throws(Throwable::class)
    override fun initZygote(startupParam: IXposedHookZygoteInit.StartupParam) {
        // keep
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
            patchDRM()
        }
    }

    @Throws(Throwable::class)
    override fun handleLoadPackage(lpparam: XC_LoadPackage.LoadPackageParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (lpparam.packageName == "miui.drm" || lpparam.packageName == "com.miui.system" || lpparam.packageName == "miui.system") {
            if (prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
                patchDRM()
            }
            return
        }
        if (lpparam.packageName == "com.android.thememanager") {
            if (prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {

                // payment
                XpUtils.findAndHookMethod("miui.resourcebrowser.view.ResourceOperationHandler", lpparam.classLoader, "isAuthorizedResource", XC_MethodReplacement.returnConstant(true))
                XpUtils.findAndHookMethod("miui.resourcebrowser.controller.online.NetworkHelper", lpparam.classLoader, "validateResponseResult", Integer.TYPE, InputStream::class.java, object : XC_MethodHook() {
                    @Throws(Throwable::class)
                    override fun beforeHookedMethod(param: MethodHookParam) {
                        param.result = param.args[1] as InputStream?
                    }
                })
                XpUtils.findAndHookMethod("miui.resourcebrowser.view.ResourceOperationHandler", lpparam.classLoader, "onCheckResourceRightEventBeforeRealApply", object : XC_MethodHook() {
                    @Throws(Throwable::class)
                    override fun beforeHookedMethod(param: MethodHookParam) {
                        try { XposedHelpers.setBooleanField(param.thisObject, "mIsLegal", true) } catch (t: Throwable) { }
                    }
                })

                // validate
                val clsResource = XpUtils.findClass(lpparam.classLoader, "miui.resourcebrowser.model.Resource")
                if (clsResource != null) {
                    XpUtils.findAndHookMethod("miui.resourcebrowser.controller.online.DrmService", lpparam.classLoader, "isLegal", clsResource, XC_MethodReplacement.returnConstant(true))
                }
                XpUtils.findAndHookMethod("miui.resourcebrowser.view.ResourceOperationHandler", lpparam.classLoader, "isLegal", XC_MethodReplacement.returnConstant(true))
                XpUtils.findAndHookMethod("miui.resourcebrowser.view.ResourceOperationHandler", lpparam.classLoader, "onCheckResourceRightEventBeforeRealApply", object : XC_MethodHook() {
                    @Throws(Throwable::class)
                    override fun beforeHookedMethod(param: MethodHookParam) {
                        try { XposedHelpers.setBooleanField(param.thisObject, "mIsLegal", true) } catch (t: Throwable) { }
                    }
                })

                // notification
                XpUtils.findAndHookMethod("miui.resourcebrowser.model.Resource", lpparam.classLoader, "isProductBought", XC_MethodReplacement.returnConstant(true))
                XpUtils.findAndHookMethod("miui.resourcebrowser.model.ResourceOnlineProperties", lpparam.classLoader, "setProductBought", java.lang.Boolean.TYPE, object : XC_MethodReplacement() {
                    @Throws(Throwable::class)
                    override fun replaceHookedMethod(param: MethodHookParam): Any? {
                        param.args = arrayOf(true)
                        return XposedBridge.invokeOriginalMethod(param.method, param.thisObject, arrayOf(true))
                    }
                })

                XpUtils.findAndHookMethod("miui.resourcebrowser.model.ResourceOnlineProperties", lpparam.classLoader, "isProductBought", XC_MethodReplacement.returnConstant(true))

                // common
                XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isTrialable", XC_MethodReplacement.returnConstant(false))
                XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isLegal", XC_MethodReplacement.returnConstant(true))
                XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isAuthorizedResource", XC_MethodReplacement.returnConstant(true))
                XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isPermanentRights", XC_MethodReplacement.returnConstant(true))
            }
        }
    }

    private fun patchDRM() {

        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", Context::class.java, File::class.java, File::class.java, XC_MethodReplacement.returnConstant(drmResultSUCCESS))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", Context::class.java, String::class.java, File::class.java, XC_MethodReplacement.returnConstant(drmResultSUCCESS))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isPermanentRights", File::class.java, XC_MethodReplacement.returnConstant(true))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isRightsFileLegal", File::class.java, XC_MethodReplacement.returnConstant(true))


        val clsRightObject = Class.forName("miui.drm.DrmManager\$RightObject")
        if (clsRightObject != null) {
            XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", Context::class.java, String::class.java, clsRightObject, XC_MethodReplacement.returnConstant(drmResultSUCCESS))
            XpUtils.findAndHookMethod("miui.drm.DrmManager", "isPermanentRights", clsRightObject, XC_MethodReplacement.returnConstant(true))
            XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", clsRightObject, XC_MethodReplacement.returnConstant(drmResultSUCCESS))
        }

        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isSupportAd", Context::class.java, XC_MethodReplacement.returnConstant(false))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isSupportAd", File::class.java, XC_MethodReplacement.returnConstant(false))

        XpUtils.findAndHookMethod("miui.drm.DrmManager", "setSupportAd", Context::class.java, java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                param.args[1] = false
            }
        })

        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", File::class.java, File::class.java, XC_MethodReplacement.returnConstant(drmResultSUCCESS))
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", String::class.java, File::class.java, XC_MethodReplacement.returnConstant(drmResultSUCCESS))

        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "validateTheme", Context::class.java, String::class.java, String::class.java, XC_MethodReplacement.returnConstant(drmResultSUCCESS))
        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "restoreDefault", XC_MethodReplacement.returnConstant(null))
        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "onReceive", Context::class.java, Intent::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                val intent = param.args[1] as Intent?
                if (intent != null && intent.action != null && intent.action == "miui.intent.action.CHECK_TIME_UP") {
                    intent.action = ""
                    param.args[1] = intent
                }
            }
        })

        XpUtils.findAndHookMethod("miui.content.res.ThemeRuntimeManager\$ThemeReceiver", "validateTheme", Context::class.java, String::class.java, String::class.java, XC_MethodReplacement.returnConstant(drmResultSUCCESS))
        XpUtils.findAndHookMethod("miui.content.res.ThemeRuntimeManager", "restoreDefault", XC_MethodReplacement.returnConstant(null))

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

