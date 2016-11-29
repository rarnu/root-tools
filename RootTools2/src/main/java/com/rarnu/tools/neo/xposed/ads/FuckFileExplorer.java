package com.rarnu.tools.neo.xposed.ads;

import android.content.Context;
import com.rarnu.tools.neo.xposed.XpUtils;
import de.robv.android.xposed.XC_MethodReplacement;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

/**
 * Created by rarnu on 11/18/16.
 */
public class FuckFileExplorer {

    public static void fuckFileExplorer(XC_LoadPackage.LoadPackageParam loadPackageParam) {
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "isAdEnable", Context.class, String.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "supportAd", XC_MethodReplacement.returnConstant(false));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "ifAdShowByCloudForNetwork", Context.class, String.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "getHomePageHotVideoTipSwitch", Context.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "getHomePageHotVideoTopicUri", Context.class, XC_MethodReplacement.returnConstant(""));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "getAdStyleName", Context.class, String.class, XC_MethodReplacement.returnConstant(""));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "tryInit", Context.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "isVideoEnable", Context.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.model.ConfigHelper", loadPackageParam.classLoader, "isStickerEnable", Context.class, XC_MethodReplacement.returnConstant(false));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.util.XLUtil", loadPackageParam.classLoader, "isNetworkAvailable", Context.class, XC_MethodReplacement.returnConstant(false));

        XpUtils.INSTANCE.findAndHookMethod("com.xunlei.adlibrary.XunleiADSdk", loadPackageParam.classLoader, "setup", Context.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.INSTANCE.findAndHookMethod("com.xunlei.adlibrary.analytics.xunlei.AdStatHelper", loadPackageParam.classLoader, "init", Context.class, XC_MethodReplacement.returnConstant(null));
        XpUtils.INSTANCE.findAndHookMethod("com.android.fileexplorer.video.upload.VideoItemManager", loadPackageParam.classLoader, "initLoad", XC_MethodReplacement.returnConstant(null));
    }

}
