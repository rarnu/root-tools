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

    public static void setStaticBooleanField(String clsName, ClassLoader loader, String field, boolean value) {
        try {
            XposedHelpers.setStaticBooleanField(loader.loadClass(clsName), field, value);
        } catch (Throwable th) {
            XposedBridge.log("RootToolsNeo setStaticBooleanField: " + th.toString());
        }
    }

    public static void setReplacement(XC_InitPackageResources.InitPackageResourcesParam param, String clsName, String type, String name, Object value) {
        try {
            param.res.setReplacement(clsName, type, name, value);
        } catch (Throwable th) {
            XposedBridge.log("RootToolsNeo setReplacement: " + th.toString());
        }
    }

    public static Class<?> findClass(ClassLoader loader, String clsName) {
        Class<?> cls = null;
        try {
            cls = loader.loadClass(clsName);
        } catch (Throwable th) {
            XposedBridge.log("RootToolsNeo findClass: " + th.toString());
        }
        return cls;
    }
}
