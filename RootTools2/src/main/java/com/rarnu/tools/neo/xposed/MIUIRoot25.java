package com.rarnu.tools.neo.xposed;

import android.os.Bundle;
import android.os.Message;
import android.widget.Button;
import android.widget.TextView;
import de.robv.android.xposed.*;
import de.robv.android.xposed.callbacks.XC_InitPackageResources;
import de.robv.android.xposed.callbacks.XC_LayoutInflated;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

public class MIUIRoot25 implements IXposedHookInitPackageResources, IXposedHookLoadPackage {

    public static TextView warningText;
    public static Button accept;

    @Override
    public void handleLoadPackage(XC_LoadPackage.LoadPackageParam loadPackageParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (!prefs.getBoolean(XpStatus.KEY_ROOTCRACK, false)) {
            return;
        }
        if (!loadPackageParam.packageName.equals("com.miui.securitycenter")) {
            return;
        }
        XpUtils.findAndHookMethod("com.miui.permcenter.root.RootApplyActivity", loadPackageParam.classLoader, "onCreate", Bundle.class, new XC_MethodHook() {
            protected void afterHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                if (accept == null) {
                    return;
                }
                int i = 0;
                while (i < 5) {
                    accept.performClick();
                    i += 1;
                }
            }
        });
        XpUtils.findAndHookMethod("com.miui.permcenter.root.c", loadPackageParam.classLoader, "handleMessage", Message.class, new XC_MethodReplacement() {
            protected Object replaceHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                return null;
            }
        });
        XpUtils.findAndHookMethod("com.miui.permcenter.root.a", loadPackageParam.classLoader, "handleMessage", Message.class, new XC_MethodReplacement() {
            protected Object replaceHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                return null;
            }
        });
    }


    @Override
    public void handleInitPackageResources(XC_InitPackageResources.InitPackageResourcesParam initPackageResourcesParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (!prefs.getBoolean(XpStatus.KEY_ROOTCRACK, false)) {
            return;
        }
        if (!initPackageResourcesParam.packageName.equals("com.miui.securitycenter")) {
            return;
        }
        initPackageResourcesParam.res.hookLayout("com.miui.securitycenter", "layout", "pm_activity_root_apply", new XC_LayoutInflated() {
            public void handleLayoutInflated(XC_LayoutInflated.LayoutInflatedParam paramAnonymousLayoutInflatedParam) throws Throwable {
                accept = (Button) paramAnonymousLayoutInflatedParam.view.findViewById(paramAnonymousLayoutInflatedParam.res.getIdentifier("accept", "id", "com.miui.securitycenter"));
                warningText = (TextView) paramAnonymousLayoutInflatedParam.view.findViewById(paramAnonymousLayoutInflatedParam.res.getIdentifier("warning_info", "id", "com.miui.securitycenter"));
                if (warningText != null) {
                    warningText.setLines(6);
                }
            }
        });
    }

}
