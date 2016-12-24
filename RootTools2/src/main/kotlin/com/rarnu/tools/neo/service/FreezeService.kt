package com.rarnu.tools.neo.service

import android.app.Service
import android.content.Intent
import android.os.IBinder
import com.rarnu.tools.neo.api.NativeAPI
import kotlin.concurrent.thread

/**
 * Created by rarnu on 12/24/16.
 */
class FreezeService : Service() {

    override fun onBind(intent: Intent?): IBinder? = null

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        thread { NativeAPI.freezeOnLoad() }
        return super.onStartCommand(intent, flags, startId)
    }
}