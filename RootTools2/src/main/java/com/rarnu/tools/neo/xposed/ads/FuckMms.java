package com.rarnu.tools.neo.xposed.ads;

import android.content.Context;
import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckMms {

    public static void fuckMms(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        XpUtils.INSTANCE.findAndHookMethod("com.android.mms.ui.MessageUtils", loadPackageParam.classLoader, "isMessagingTemplateAllowed", Context.class, new XC_MethodHook() {
            protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                Context mc = (Context) paramAnonymousMethodHookParam.args[0];
                paramAnonymousMethodHookParam.setResult(!mc.getClass().getName().toLowerCase().contains("app"));
            }
        });
        XpUtils.INSTANCE.findAndHookMethod("com.android.mms.ui.SingleRecipientConversationActivity", loadPackageParam.classLoader, "showMenuMode", new XC_MethodReplacement() {
            @Override
            protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                return null;
            }
        });
        XpUtils.INSTANCE.findAndHookMethod("com.android.mms.util.MiStatSdkHelper", loadPackageParam.classLoader, "recordBottomMenuShown", String.class, new XC_MethodReplacement() {
            @Override
            protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                return null;
            }
        });
    }
}
