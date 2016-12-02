package com.rarnu.tools.neo.data

import android.content.Context
import android.os.Handler
import android.os.Message
import com.rarnu.tools.neo.api.API
import org.json.JSONException
import org.json.JSONObject
import kotlin.concurrent.thread

class UpdateInfo  {

    interface UpdateInfoReadyCallback {
        fun onUpdateInfoReady(info: UpdateInfo?)
    }

    var versionName = ""
    var versionCode = 0
    var description = ""
    var url = ""

    private var isReady = false
    private var callback: UpdateInfoReadyCallback? = null

    private val hCallback = object : Handler() {
        override fun handleMessage(msg: Message) {
            callback?.onUpdateInfoReady(_instance)
            super.handleMessage(msg)
        }
    }

    private constructor(callback: UpdateInfo.UpdateInfoReadyCallback?) {
        _instance = this
        this.callback = callback
        threadInitUpdateInfo()
    }

    private fun threadInitUpdateInfo() {
        isReady = false
        thread {
            val jsonStr = API.getUpdateInfo()
            try {
                val json = JSONObject(jsonStr)
                versionCode = json.getInt("versionCode")
                versionName = json.getString("versionName")
                url = json.getString("url")
                description = json.getString("description")
            } catch (e: JSONException) {

            }

            isReady = true
            hCallback.sendEmptyMessage(0)
        }
    }

    fun isNewVersion(ctx: Context?): Boolean {
        var ret = false
        try {
            val pm = ctx?.packageManager
            val pkg = pm?.getPackageInfo(ctx?.packageName, 0)
            val verCode = pkg?.versionCode
            ret = versionCode > verCode!!
        } catch (e: Exception) {
        }
        return ret
    }

    companion object {
        private var _instance: UpdateInfo? = null
        fun getUpdateInfo(callback: UpdateInfoReadyCallback?): UpdateInfo? {
            UpdateInfo(callback)
            return _instance
        }
    }

}
