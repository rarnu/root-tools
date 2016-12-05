package com.rarnu.tools.neo.api

import android.util.Log
import com.rarnu.tools.neo.data.Onekey
import com.rarnu.tools.neo.utils.HttpUtils
import org.json.JSONObject

import java.util.HashMap
import java.util.StringTokenizer

object API {

    private val API_BASE = "http://diy.ourocg.cn/root/"
    val DOWNLOAD_URL = API_BASE + "download/"

    fun getUpdateInfo(): String? = HttpUtils.get(API_BASE + "version.php", "")

    fun getOnekey(pkgName: String?, versionCode: Int): Onekey? {
        val str = HttpUtils.get(API_BASE + "onekey.php", "action=get&pkg=$pkgName&ver=$versionCode")
        var ok: Onekey? = null
        if (str != null && str.trim { it <= ' ' } != "") {
            ok = Onekey(pkgName, str)
        }
        return ok
    }

    fun uploadOnekey(pkgName: String?, versionCode: Int, disabled: List<String?>?): Boolean {
        // upload onekey
        val param = HashMap<String, String?>()
        param.put("action", "put")
        param.put("pkg", pkgName)
        param.put("ver", versionCode.toString())
        var data = ""
        if (disabled != null && disabled.size != 0) {
            for (s in disabled) {
                data += s + "\n"
            }
        }
        param.put("data", data)
        val str = HttpUtils.post(API_BASE + "onekey.php", param)
        return str == "OK"
    }

    fun sendFeedback(nickname: String, comment: String, photo: Array<String>): Boolean {
        val params = HashMap<String, String>()
        params.put("nickname", nickname)
        params.put("comment", comment)
        val files = HashMap<String, String>()
        for (i in photo.indices) {
            if (photo[i] != "") {
                files.put("photo${i + 1}", photo[i])
            }
        }
        val data = HttpUtils.postFile(API_BASE + "feedback.php", params, files)
        Log.e("API", "sendFeedback => $data")
        var ret = false
        try {
            val json = JSONObject(data)
            ret = json.getInt("result") == 0
        } catch (e: Exception) {

        }
        return ret
    }

    fun reportCrash() {
        // TODO:
    }

}
