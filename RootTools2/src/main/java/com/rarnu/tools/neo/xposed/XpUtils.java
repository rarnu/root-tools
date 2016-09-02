package com.rarnu.tools.neo.xposed;

import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers;

public class XpUtils {

    public static void findAndHookMethod(String clsName, ClassLoader loader, String method, Object ... parameterTypesAndCallback) {
        try {
            XposedHelpers.findAndHookMethod(clsName, loader, method, parameterTypesAndCallback);
        } catch (Throwable th) {
            XposedBridge.log(th.toString());
        }

    }

    public static void findAndHookMethod(String clsName, String method, Object[] parameter) {
        try {
            XposedHelpers.findAndHookMethod(Class.forName(clsName), method, parameter);
        } catch (Throwable th) {
            XposedBridge.log(th.toString());
        }
    }

    public static void setStaticBooleanField(String clsName, String field, boolean value) {
        try {
            XposedHelpers.setStaticBooleanField(Class.forName(clsName), field, value);
        } catch (Throwable th) {
            XposedBridge.log(th.toString());
        }
    }

}
