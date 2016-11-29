package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/18/16.
 */
object FuckDownload {

    fun fuckDownload(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("com.android.providers.downloads.ui.recommend.config.ADConfig", loadPackageParam.classLoader, "OSSupportAD", object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam?) {
                paramAnonymousMethodHookParam!!.result = false
            }
        })
        XpUtils.findAndHookMethod("com.android.providers.downloads.ui.utils.BuildUtils", loadPackageParam.classLoader, "isCmTestBuilder", XC_MethodReplacement.returnConstant(true))
    }
}
