package com.rarnu.tools.neo.xposed.ads

import com.rarnu.xfunc.*

/**
 * Created by rarnu on 12/8/16.
 */
object FuckContacts {
    fun fuckContacts(pkg: XposedPkg) {
        // fuck contacts
        pkg.findClass("com.android.contacts.yellowpage.ui.NavigationFragment").findMethod("setIsAdOn", java.lang.Boolean.TYPE).hook {
            before {
                args[0] = false
            }
        }

        val clsServiceDataEntry = pkg.findClass("miui.yellowpage.ServicesDataEntry")
        if (clsServiceDataEntry != null) {
            pkg.findClass("com.android.contacts.yellowpage.adapter.YellowPageAdapter").findMethod("updateData", List::class.java).hook {
                before {
                    val list = args[0] as MutableList<*>?
                    val mGetItemType = clsServiceDataEntry.getDeclaredMethod("getItemType").apply { isAccessible = true }
                    val newList = mutableListOf<Any?>()
                    if (list != null) {
                        for (item in list) {
                            val typ = mGetItemType.invoke(item) as Enum<*>
                            if (typ.ordinal != 2 && typ.ordinal != 4 && typ.ordinal != 6) {
                                newList.add(item)
                            }
                        }
                    }
                    args[0] = newList
                }
            }
        }
        pkg.findClass("com.android.contacts.ContactsUtils").findMethod("showFraudInsurance").hook { replace { result = false } }
        pkg.findClass("com.android.contacts.activities.UnknownContactActivity").findMethod("updateDetailInsurance").hook { replace { result = null } }
        pkg.findClass("com.android.contacts.detail.UnknownContactFragment").apply {
            findMethod("showInsuranceView").hook { replace { result = null } }
            findMethod("updateFraudInsurance").hook { replace { result = null } }
        }
    }
}