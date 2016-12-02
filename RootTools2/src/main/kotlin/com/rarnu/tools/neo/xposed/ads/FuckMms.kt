package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XC_MethodReplacement
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 11/18/16.
 */
object FuckMms {

    fun fuckMms(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        XpUtils.findAndHookMethod("com.android.mms.ui.MessageUtils", loadPackageParam.classLoader, "isMessagingTemplateAllowed", Context::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                val mc = paramAnonymousMethodHookParam.args[0] as Context
                paramAnonymousMethodHookParam.result = !mc.javaClass.name.toLowerCase().contains("app")
            }
        })
        XpUtils.findAndHookMethod("com.android.mms.ui.SingleRecipientConversationActivity", loadPackageParam.classLoader, "showMenuMode", object : XC_MethodReplacement() {
            @Throws(Throwable::class)
            override fun replaceHookedMethod(param: XC_MethodHook.MethodHookParam): Any? {
                return null
            }
        })
        XpUtils.findAndHookMethod("com.android.mms.util.MiStatSdkHelper", loadPackageParam.classLoader, "recordBottomMenuShown", String::class.java, object : XC_MethodReplacement() {
            @Throws(Throwable::class)
            override fun replaceHookedMethod(param: XC_MethodHook.MethodHookParam): Any? {
                return null
            }
        })
    }
}
