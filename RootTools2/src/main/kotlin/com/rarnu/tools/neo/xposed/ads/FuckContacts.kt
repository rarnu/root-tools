package com.rarnu.tools.neo.xposed.ads

import com.rarnu.tools.neo.xposed.XpUtils
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.callbacks.XC_LoadPackage

/**
 * Created by rarnu on 12/8/16.
 */
object FuckContacts {
    fun fuckContacts(loadPackageParam: XC_LoadPackage.LoadPackageParam) {
        // fuck contacts
        XpUtils.findAndHookMethod("com.android.contacts.yellowpage.ui.NavigationFragment", loadPackageParam.classLoader, "setIsAdOn", java.lang.Boolean.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(param: MethodHookParam) {
                param.args[0] = false
            }
        })
        val clsServiceDataEntry = XpUtils.findClass(loadPackageParam.classLoader, "miui.yellowpage.ServicesDataEntry")
        if (clsServiceDataEntry != null) {
            XpUtils.findAndHookMethod("com.android.contacts.yellowpage.adapter.YellowPageAdapter", loadPackageParam.classLoader, "updateData", List::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val list = param.args[0] as MutableList<*>?
                    val mGetItemType = clsServiceDataEntry.getDeclaredMethod("getItemType")

                    mGetItemType.isAccessible = true
                    val newList = arrayListOf<Any?>()
                    if (list != null) {
                        for (item in  list) {
                            val typ = mGetItemType.invoke(item) as Enum<*>
                            if (typ.ordinal != 2 && typ.ordinal != 4 && typ.ordinal != 6) {
                                newList.add(item)
                            }
                        }
                    }
                    param.args[0] = newList
                }
            })
        }
    }
}