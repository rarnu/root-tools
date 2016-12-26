package com.rarnu.tools.neo.receiver

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.os.Build
import android.util.Log
import com.rarnu.tools.neo.service.FreezeService
import com.rarnu.tools.neo.xposed.XpStatus

/**
 * Created by rarnu on 12/24/16.
 */
class BootReceiver : BroadcastReceiver() {

    companion object {
        var inited = false
    }

    override fun onReceive(context: Context?, intent: Intent?) {
        Log.e("BootReceiver", "Receive boot message => ${intent?.action}")
        if (!inited) {
            inited = true
            val pref = context?.getSharedPreferences(XpStatus.PREF, if (Build.VERSION.SDK_INT < 24) 1 else 0)
            if (pref!!.getBoolean(XpStatus.KEY_PREVENT_FREEZE_REVERSE, false)) {
                context?.startService(Intent(context, FreezeService::class.java))
            }
        }
    }
}