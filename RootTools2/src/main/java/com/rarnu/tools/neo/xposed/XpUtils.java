package com.rarnu.tools.neo.xposed;

import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers;
import de.robv.android.xposed.callbacks.XC_InitPackageResources;

public class XpUtils {

    public static void findAndHookMethod(String clsName, ClassLoader loader, String method, Object ... parameterTypesAndCallback) {
        try {
            XposedHelpers.findAndHookMethod(clsName, loader, method, parameterTypesAndCallback);
        } catch (Throwable th) {
            XposedBridge.log("RootToolsNeo findAndHookMethod: " + th.toString());
        }

    }

    public static void findAndHookMethod(String clsName, String method, Object[] parameter) {
        try {
            XposedHelpers.findAndHookMethod(Class.forName(clsName), method, parameter);
        } catch (Throwable th) {
            XposedBridge.log("RootToolsNeo findAndHookMethod: " + th.toString());
        }
    }

    public static void setStaticBooleanField(String clsName, String field, boolean value) {
        try {
            XposedHelpers.setStaticBooleanField(Class.forName(clsName), field, value);
        } catch (Throwable th) {
            XposedBridge.log("RootToolsNeo setStaticBooleanField: " + th.toString());
        }
    }

    public static void setReplacement(XC_InitPackageResources.InitPackageResourcesParam param, String clsName, String type, String name, Object value) {
        try {
            param.res.setReplacement(clsName, type, name, value);
            XposedBridge.log("MIUIColumns patched!! " + clsName + ": " + name);
        } catch (Throwable th) {
            XposedBridge.log("MIUIColumns error: " + name + " ======= " + th.getMessage());
        }
    }
}
