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


    fun setStaticBooleanField(clsName: String, loader: ClassLoader, field: String, value: Boolean) = try {
        XposedHelpers.setStaticBooleanField(loader.loadClass(clsName), field, value)
    } catch (th: Throwable) {
        XposedBridge.log("RootToolsNeo setStaticBooleanField: " + th.toString())
    }

    fun setStaticBooleanField(clsName: String, field: String, value: Boolean) = try {
        XposedHelpers.setStaticBooleanField(Class.forName(clsName), field, value)
    } catch (th: Throwable) {

    }

    fun setReplacement(param: XC_InitPackageResources.InitPackageResourcesParam, clsName: String, type: String, name: String, value: Any) = try {
        param.res.setReplacement(clsName, type, name, value)
    } catch (th: Throwable) {
        XposedBridge.log("RootToolsNeo setReplacement: " + th.toString())
    }


    fun findClass(loader: ClassLoader, clsName: String): Class<*>? = try {
        loader.loadClass(clsName)
    } catch (th: Throwable) {
        XposedBridge.log("RootToolsNeo findClass: " + th.toString())
        null
    }

}
