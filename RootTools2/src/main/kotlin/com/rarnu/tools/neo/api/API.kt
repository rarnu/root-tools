package com.rarnu.tools.neo.api

import android.content.Context
import android.os.Build
import android.util.Log
import com.rarnu.tools.neo.data.Onekey
import com.rarnu.tools.neo.data.ThanksInfo
import com.rarnu.tools.neo.utils.HttpUtils
import org.jetbrains.annotations.Mutable
import org.json.JSONObject

import java.util.HashMap
import java.util.StringTokenizer

object API {

    private val API_BASE = "http://diy.ourocg.cn/root/"
    val DOWNLOAD_URL = API_BASE + "download/"
    val HEAD_URL = API_BASE + "head/"

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
        val param = hashMapOf<String, String?>()
        param.put("action", "put")
        param.put("pkg", pkgName)
        param.put("ver", versionCode.toString())
        var data = ""
        if (disabled != null && disabled.isNotEmpty()) {
            for (s in disabled) {
                data += s + "\n"
            }
        }
        param.put("data", data)
        val str = HttpUtils.post(API_BASE + "onekey.php", param)
        return str == "OK"
    }

    fun sendFeedback(nickname: String, comment: String, photo: Array<String>): Boolean {
        val params = hashMapOf<String, String?>()
        params.put("nickname", nickname)
        params.put("comment", comment)
        val files = HashMap<String, String>()
        photo.indices.filter { photo[it] != "" }.forEach { files.put("photo${it + 1}", photo[it]) }
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

    fun getThanksInfo(): MutableList<ThanksInfo?>? {
        val data = HttpUtils.get(API_BASE + "thanks.php", "")
        var list: MutableList<ThanksInfo?>? = null
        try {
            val json = JSONObject(data)
            if (json.getInt("result") == 0) {
                list = arrayListOf()
                val arr = json.getJSONArray("data")
                (0..arr.length() - 1).mapTo(list) {
                    ThanksInfo.fromJson(arr.getJSONObject(it))
                }
            }
        } catch (e: Exception) {

        }
        return list
    }

    fun reportCrash(ctx: Context?, data: String?) {
        // report crash
        val param = hashMapOf<String, String?>()
        param.put("model", Build.MODEL)
        param.put("sdk", Build.VERSION.SDK_INT.toString())
        val info = ctx?.packageManager?.getPackageInfo(ctx.packageName, 0)
        param.put("appver", info?.versionCode.toString())
        param.put("data", data)
        HttpUtils.post(API_BASE + "crash.php", param)
    }

}
