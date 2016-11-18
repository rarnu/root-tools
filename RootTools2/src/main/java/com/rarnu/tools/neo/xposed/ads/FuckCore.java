package com.rarnu.tools.neo.xposed.ads;

import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckCore {

    public static void fuckCore(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        XpUtils.findAndHookMethod("miui.os.SystemProperties", loadPackageParam.classLoader, "get", String.class, String.class, new XC_MethodHook() {

            protected void afterHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                if (paramAnonymousMethodHookParam.args[0].toString().equals("ro.product.mod_device")) {
                    paramAnonymousMethodHookParam.setResult("gemini_global");
                }
            }

            protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                if (paramAnonymousMethodHookParam.args[0].toString().equals("ro.product.mod_device")) {
                    paramAnonymousMethodHookParam.setResult("gemini_global");
                }
            }
        });
    }

}
