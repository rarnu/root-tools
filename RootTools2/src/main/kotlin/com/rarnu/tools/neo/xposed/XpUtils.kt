package com.rarnu.tools.neo.xposed

import de.robv.android.xposed.XposedBridge
import de.robv.android.xposed.XposedHelpers
import de.robv.android.xposed.callbacks.XC_InitPackageResources

object XpUtils {

    fun findAndHookMethod(clsName: String, loader: ClassLoader, method: String, vararg parameterTypesAndCallback: Any?) = try {
        XposedHelpers.findAndHookMethod(clsName, loader, method, *parameterTypesAndCallback)
    } catch (th: Throwable) {
        XposedBridge.log("RootToolsNeo findAndHookMethod: " + th.toString())
    }

    fun findAndHookMethod(clsName: String?, method: String?, vararg parameterTypesAndCallback: Any?) = try {
        XposedHelpers.findAndHookMethod(Class.forName(clsName), method, *parameterTypesAndCallback)
    } catch (th: Throwable) {
        XposedBridge.log("RootToolsNeo findAndHookMethod: " + th.toString())
    }

    fun findAndHookConstructor(clsName: String, loader: ClassLoader, vararg parameterTypesAndCallback: Any?) = try {
        XposedHelpers.findAndHookConstructor(clsName, loader, *parameterTypesAndCallback)
    } catch (th: Throwable) {
        XposedBridge.log("RootToolsNeo findAndHookConstructor: " + th.toString())
    }

    fun findClass(loader: ClassLoader, clsName: String): Class<*>? = try {
        loader.loadClass(clsName)
    } catch (th: Throwable) {
        XposedBridge.log("RootToolsNeo findClass: " + th.toString())
        null
    }

}
