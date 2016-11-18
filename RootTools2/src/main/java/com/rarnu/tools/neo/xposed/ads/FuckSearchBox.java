package com.rarnu.tools.neo.xposed.ads;

import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

import java.util.List;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckSearchBox {

    public static void fuckSearchBox(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        XpUtils.findAndHookMethod("com.android.quicksearchbox.ui.LocalListView", loadPackageParam.classLoader, "updateHotQuery", List.class, int.class, new XC_MethodReplacement() {
            @Override
            protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                return null;
            }
        });
        XpUtils.findAndHookMethod("com.android.quicksearchbox.util.HotWordsUtil", loadPackageParam.classLoader, "setHotQueryView", "com.android.quicksearchbox.ui.HotQueryView", new XC_MethodHook() {
            protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                paramAnonymousMethodHookParam.setResult(null);
            }
        });
    }
}
