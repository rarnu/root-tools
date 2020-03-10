@file:Suppress("Duplicates")

package com.rarnu.tools.neo.xposed

import android.content.Context
import android.content.Intent
import com.rarnu.xfunc.*
import de.robv.android.xposed.*
import java.io.File
import java.io.InputStream
import java.lang.Exception

class MIUIThemeZygote: XposedZygote() {
    override fun hook(zygote: XposedStartup) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
            patchDRM(null)
        }
    }
}

class MIUIThemePackage: XposedPackage() {
    override fun hook(pkg: XposedPkg) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (pkg.packageName == "miui.drm" || pkg.packageName == "com.miui.system" || pkg.packageName == "miui.system") {
            if (prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
                patchDRM(pkg.classLoader)
            }
            return
        }
        if (pkg.packageName == "com.android.thememanager") {
            if (prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
                pkg.findClass("miui.resourcebrowser.view.ResourceOperationHandler").findMethod("isAuthorizedResource").hook { replace { result = true } }
                // payment
                pkg.findClass("miui.resourcebrowser.controller.online.NetworkHelper").findMethod("validateResponseResult", Integer.TYPE, InputStream::class.java).hook {
                    before {
                        result = args[1] as InputStream?
                    }
                }
                pkg.findClass("miui.resourcebrowser.view.ResourceOperationHandler").findMethod("onCheckResourceRightEventBeforeRealApply").hook {
                    before {
                        try {
                            thisObject.javaClass.findField("mIsLegal").setStatic(true)
                        } catch (t: Throwable) {
                        }
                    }
                }
                pkg.findClass("miui.resourcebrowser.view.ResourceOperationHandler").findMethod("onCheckResourceRightEventBeforeRealApply").hook {
                    before {
                        try {
                            thisObject.javaClass.findField("mIsLegal").setStatic(true)
                        } catch (t: Throwable) {
                        }
                    }
                }
                val clsResource = pkg.findClass("miui.resourcebrowser.model.Resource")
                if (clsResource != null) {
                    pkg.findClass("miui.resourcebrowser.controller.online.DrmService").findMethod("isLegal", clsResource).hook { replace { result = true } }
                }
                pkg.findClass("miui.resourcebrowser.view.ResourceOperationHandler").apply {
                    findMethod("isLegal").hook { replace { result = true } }
                    findMethod("onCheckResourceRightEventBeforeRealApply").hook {
                        before {
                            try {
                                thisObject.javaClass.findField("mIsLegal").setStatic(true)
                            } catch (t: Throwable) {
                            }
                        }
                    }
                }
                pkg.findClass("miui.resourcebrowser.model.Resource").findMethod("isProductBought").hook { replace { result = true } }
                pkg.findClass("miui.resourcebrowser.model.ResourceOnlineProperties").findMethod("setProductBought", java.lang.Boolean.TYPE).hook {
                    replace {
                        args = arrayOf(true)
                        result = XposedBridge.invokeOriginalMethod(method, thisObject, arrayOf(true))
                    }
                }
                pkg.findClass("miui.resourcebrowser.model.ResourceOnlineProperties").findMethod("isProductBought").hook { replace { result = true } }
                pkg.findClass("com.android.thememanager.util.ThemeOperationHandler").apply {
                    findMethod("isTrialable").hook { replace { result = false } }
                    findMethod("isLegal").hook { replace { result = true } }
                    findMethod("isAuthorizedResource").hook { replace { result = true } }
                    findMethod("isPermanentRights").hook { replace { result = true } }
                }
            }
        }
    }
}

private fun patchDRM(cl: ClassLoader?) {
    fun findClass(clsName: String) = XposedHelpers.findClass(clsName, cl)
    findClass("miui.drm.DrmManager").apply {
        findMethod("isLegal", Context::class.java, File::class.java, File::class.java).hook { replace { result= drmResultSUCCESS } }
        findMethod("isLegal", Context::class.java, String::class.java, File::class.java).hook { replace { result = drmResultSUCCESS } }
        findMethod("isPermanentRights", File::class.java).hook { replace { result = true } }
        findMethod("isRightsFileLegal", File::class.java).hook { replace { result = true } }
        try {
            val clsRightObject = Class.forName("miui.drm.DrmManager\$RightObject")
            findMethod("isLegal", Context::class.java, String::class.java, clsRightObject).hook { replace { result = drmResultSUCCESS } }
            findMethod("isPermanentRights", clsRightObject).hook { replace { result = true } }
            findMethod("isLegal", clsRightObject).hook { replace { result = drmResultSUCCESS } }
        } catch (e: Exception) {

        }
        findMethod("isSupportAd", Context::class.java).hook { replace { result = false } }
        findMethod("isSupportAd", File::class.java).hook { replace { result = false } }
        findMethod("setSupportAd", Context::class.java, java.lang.Boolean.TYPE).hook {
            before {
                args[1] = false
            }
        }
        findMethod("isLegal", File::class.java, File::class.java).hook { replace { result = drmResultSUCCESS } }
        findMethod("isLegal", String::class.java, File::class.java).hook { replace { result = drmResultSUCCESS } }
    }

    findClass("miui.drm.ThemeReceiver").apply {
        findMethod("validateTheme", Context::class.java, String::class.java, String::class.java).hook { replace { result = drmResultSUCCESS } }
        findMethod("restoreDefault").hook { replace { result = null } }
        findMethod("onReceive", Context::class.java, Intent::class.java).hook {
            before {
                val intent = args[1] as Intent?
                if (intent != null && intent.action != null && intent.action == "miui.intent.action.CHECK_TIME_UP") {
                    intent.action = ""
                    args[1] = intent
                }
            }
        }
    }
    findClass("miui.content.res.ThemeRuntimeManager\$ThemeReceiver").findMethod("validateTheme", Context::class.java, String::class.java, String::class.java).hook { replace { result = drmResultSUCCESS } }
    findClass("miui.content.res.ThemeRuntimeManager").findMethod("restoreDefault").hook { replace { result = null } }

}

val drmResultSUCCESS: Any?
    get() {
        try {
            val clsEnum = Class.forName("miui.drm.DrmManager\$DrmResult")
            val mEnum = clsEnum.getDeclaredMethod("valueOf", String::class.java)
            return mEnum.invoke(null, "DRM_SUCCESS")
        } catch (th: Throwable) {
            XposedBridge.log(th.toString())

        }
        return null
    }

