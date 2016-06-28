package com.rarnu.mi8

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent

/**
 * Created by rarnu on 6/27/16.
 */
class PresentReceiver: BroadcastReceiver() {

    override fun onReceive(context: Context?, intent: Intent?) {
        if (Config.isWatching(context!!)) {
            context.startService(Intent(context, ObserverService::class.java))
        }
    }

}