package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 12/20/16.
 */
object FuckCloudService {

    fun fuckCloudService(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        // old
        XpUtils.findAndHookMethod("com.miui.cloudservice.ui.x", loadPackageParam.classLoader, "hd", XC_MethodReplacement.returnConstant(true))
        // new
        XpUtils.findAndHookMethod("com.miui.cloudservice.ui.x", loadPackageParam.classLoader, "hg", XC_MethodReplacement.returnConstant(true))
    }

}