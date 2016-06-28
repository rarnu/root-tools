package com.rarnu.mi8

import android.app.Notification
import android.app.Service
import android.content.Context
import android.content.Intent
import android.os.FileObserver
import android.os.IBinder
import android.util.Log
import kotlin.concurrent.thread

/**
 * Created by rarnu on 6/27/16.
 */
class ObserverService: Service() {

    companion object {
        var isRunning = false
        val NOTIFY_ID = 401
    }

    private var obWeiXin: MutableList<MI8Observer>? = null
    private var obQQ: MutableList<MI8Observer>? = null

    override fun onBind(intent: Intent?): IBinder? = null

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        Log.e("ObserverService", "onStartCommand => START")
        val n = Notification.Builder(applicationContext)
                .setSmallIcon(R.drawable.notify)
                .setTicker(getString(R.string.app_name))
                .setWhen(System.currentTimeMillis())
                .setContentTitle(getString(R.string.app_name))
                .setContentText(getString(R.string.notification_desc))
                .build()
        n.flags = n.flags or Notification.FLAG_ONGOING_EVENT
        startForeground(NOTIFY_ID, n)

        val emuPaths = UserUtils.getUsersPath(UserUtils.getUserIdList())
        if (Config.isDoubleWeiXin(baseContext)) {
            if (obWeiXin == null) {
                obWeiXin = arrayListOf()
                for (path in PathDefine.WEIXIN_PATH) {
                    for (p  in emuPaths) {
                        val obpath = "$p$path"
                        Log.e("ObserverService", "watch => $obpath")
                        val ob = MI8Observer(applicationContext, obpath, "${UserUtils.zeroPath}$path")
                        ob.startWatching()
                        obWeiXin?.add(ob)
                    }
                }
            }
        }
        if (Config.isDoubleQQ(baseContext)) {
            if (obQQ == null) {
                obQQ = arrayListOf()
                for (path in PathDefine.QQ_PATH) {
                    for (p in emuPaths) {
                        val obpath = "$p$path"
                        Log.e("ObserverService", "watch => $obpath")
                        val ob = MI8Observer(applicationContext, obpath, "${UserUtils.zeroPath}$path")
                        ob.startWatching()
                        obQQ?.add(ob)
                    }
                }
            }
        }
        isRunning = true
        sendBroadcast(Intent(MainActivity.ACTION_BUTTON_TITLE))
        return START_STICKY
    }

    override fun onDestroy() {
        stopForeground(true)
        if (obWeiXin != null) {
            for (ob in obWeiXin!!) {
                ob.stopWatching()
            }
            obWeiXin?.clear()
            obWeiXin = null
        }

        if (obQQ != null) {
            for (ob in obQQ!!) {
                ob.stopWatching()
            }
            obQQ?.clear()
            obQQ = null
        }
        isRunning = false
        sendBroadcast(Intent(MainActivity.ACTION_BUTTON_TITLE))
        super.onDestroy()
    }

    class MI8Observer: FileObserver {

        private var _context: Context? = null
        private var _basePath: String? = ""
        private var _baseDest: String? = ""

        constructor(context: Context, path: String?, dest: String?): super(path) {
            _context = context
            _basePath = path
            _baseDest = dest
        }

        override fun onEvent(event: Int, path: String?) {
            val action = event and FileObserver.ALL_EVENTS
            when (action) {
                FileObserver.CREATE -> {
                    val src = "$_basePath$path"
                    var dest = "$_baseDest$path"
                    Log.e("MI8Observer", "CREATE => $src")
                    thread {
                        FileUtils.copyFile(src, dest, null)
                        FileUtils.sendScanCmd(_context!!, _baseDest!!)
                    }
                }
                FileObserver.DELETE -> {
                    val src = "$_basePath$path"
                    var dest = "$_baseDest$path"
                    Log.e("MI8Observer", "DELETE => $src")
                    thread {
                        FileUtils.deleteFile(dest)
                        FileUtils.sendScanCmd(_context!!, _baseDest!!)
                    }
                }
            }
        }
    }

}