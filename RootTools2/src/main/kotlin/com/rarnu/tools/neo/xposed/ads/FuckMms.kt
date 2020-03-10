package com.rarnu.tools.neo.xposed.ads

import android.content.Context
import com.rarnu.xfunc.*

/**
 * Created by rarnu on 11/18/16.
 */
object FuckMms {

    fun fuckMms(pkg: XposedPkg) {
        pkg.findClass("com.android.mms.ui.MessageUtils").findMethod("isMessagingTemplateAllowed", Context::class.java).hook {
            before {
                val mc = args[0] as Context
                result = !mc.javaClass.name.toLowerCase().contains("app")
            }
        }
        pkg.findClass("com.android.mms.ui.SingleRecipientConversationActivity").findMethod("showMenuMode").hook { replace { result = null } }
        pkg.findClass("com.android.mms.util.MiStatSdkHelper").findMethod("recordBottomMenuShown", String::class.java).hook { replace { result = null } }
    }
}
