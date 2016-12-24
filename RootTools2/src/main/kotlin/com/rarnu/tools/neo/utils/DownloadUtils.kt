package com.rarnu.tools.neo.utils

import android.os.Handler
import android.os.Message
import com.rarnu.tools.neo.base.BaseAdapter
import java.io.File
import java.io.FileOutputStream
import java.net.HttpURLConnection
import java.net.URL
import kotlin.concurrent.thread

/**
 * Created by rarnu on 12/5/16.
 */
object DownloadUtils {

    data class DownloadInfo(var url: String, var local: String, var callback: BaseAdapter<*>?)

    val lstDownload = arrayListOf<DownloadInfo>()
    private var running = false

    fun startDownload(url: String, localFile: String, callback: BaseAdapter<*>?) {
        lstDownload.add(DownloadInfo(url, localFile, callback))
        run()
    }

    private fun run() {
        if (running) {
            return
        }
        if (lstDownload.size > 0) {
            doDownload(lstDownload[0])
        }
    }

    private fun doDownload(info: DownloadInfo) {
        val hDownload = object : Handler() {
            override fun handleMessage(msg: Message?) {
                try {
                    val i = lstDownload[0]
                    i.callback?.notifyDataSetChanged()
                    lstDownload.removeAt(0)
                    run()
                } catch (e: Exception) {
                }
                super.handleMessage(msg)
            }
        }
        thread {
            running = true
            val fTmp = File(info.local)
            if (fTmp.exists()) {
                fTmp.delete()
            }
            var url: URL?
            var position = 0
            try {
                url = URL(info.url)
                val con = url.openConnection() as HttpURLConnection
                var ins = con.inputStream
                val fileOut = File(info.local + ".tmp")
                val out = FileOutputStream(fileOut)
                val buffer = ByteArray(1024)
                var count: Int
                while (true) {
                    count = ins.read(buffer)
                    if (count != -1) {
                        out.write(buffer, 0, count)
                        position += count
                    } else {
                        break
                    }
                }
                ins.close()
                out.close()
                fileOut.renameTo(fTmp)
            } catch (e: Exception) {
            }
            running = false
            hDownload.sendEmptyMessage(1)
        }
    }

}