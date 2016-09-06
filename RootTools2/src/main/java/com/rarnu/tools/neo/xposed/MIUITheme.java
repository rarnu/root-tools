package com.rarnu.tools.neo.xposed;

import android.content.Context;
import android.content.Intent;
import de.robv.android.xposed.*;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

import java.io.File;

public class MIUITheme implements IXposedHookZygoteInit, IXposedHookLoadPackage {

    public static Object getDrmResultSUCCESS() {
        try {
            Class<Enum> clsEnum = (Class<Enum>) Class.forName("miui.drm.DrmManager$DrmResult");
            if (clsEnum != null) {
                return Enum.valueOf(clsEnum, "DRM_SUCCESS");
            }
        } catch (Throwable th) {
            XposedBridge.log(th.toString());

        }
        return null;
    }

    @Override
    public void initZygote(StartupParam startupParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (!prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
            return;
        }
        MIUI_DRM();
    }

    @Override
    public void handleLoadPackage(XC_LoadPackage.LoadPackageParam lpparam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (!prefs.getBoolean(XpStatus.KEY_THEMECRACK, false)) {
            return;
        }
        if (lpparam.packageName.equals("miui.drm") || lpparam.packageName.equals("com.miui.system") || lpparam.packageName.equals("miui.system")) {
            MIUI_DRM();
        }
        if (lpparam.packageName.equals("com.android.thememanager")) {
            XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isTrialable", XC_MethodReplacement.returnConstant(false));
            XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isLegal", XC_MethodReplacement.returnConstant(true));
            XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isAuthorizedResource", XC_MethodReplacement.returnConstant(true));
            XpUtils.findAndHookMethod("com.android.thememanager.util.ThemeOperationHandler", lpparam.classLoader, "isPermanentRights", XC_MethodReplacement.returnConstant(true));
        }
    }

    private void MIUI_DRM() {
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", new Object[]{Context.class, File.class, File.class, XC_MethodReplacement.returnConstant(getDrmResultSUCCESS())});
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", new Object[]{Context.class, String.class, File.class, XC_MethodReplacement.returnConstant(getDrmResultSUCCESS())});
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", new Object[]{Context.class, String.class, "miui.drm.DrmManager$RightObject", XC_MethodReplacement.returnConstant(getDrmResultSUCCESS())});
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isPermanentRights", new Object[]{File.class, XC_MethodReplacement.returnConstant(true)});
        XpUtils.findAndHookMethod("miui.drm.DrmManager", "isLegal", new Object[]{"miui.drm.DrmManager$RightObject", XC_MethodReplacement.returnConstant(true)});
        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "validateTheme", new Object[]{XC_MethodReplacement.returnConstant(true)});
        XpUtils.findAndHookMethod("miui.content.res.ThemeRuntimeManager.ThemeReceiver", "validateTheme", new Object[]{XC_MethodReplacement.returnConstant(true)});
        XpUtils.findAndHookMethod("miui.content.res.ThemeRuntimeManager", "restoreDefault", new Object[]{new XC_MethodReplacement() {
            @Override
            protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                return null;
            }
        }});
        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "restoreDefault", new Object[]{new XC_MethodReplacement() {
            @Override
            protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                return null;
            }
        }});
        XpUtils.findAndHookMethod("miui.content.res.ThemeRuntimeManager", "restoreDefault", new Object[]{new XC_MethodReplacement() {
            @Override
            protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                return null;
            }
        }});
        XpUtils.findAndHookMethod("miui.drm.ThemeReceiver", "onReceive", new Object[]{Context.class, Intent.class, new XC_MethodReplacement() {
            @Override
            protected Object replaceHookedMethod(MethodHookParam param) throws Throwable {
                return null;
            }
        }});
    }

}

