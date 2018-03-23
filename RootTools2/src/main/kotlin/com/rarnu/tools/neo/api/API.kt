package com.rarnu.tools.neo.api

import android.content.Context
import android.os.Build
import android.util.Log
import com.rarnu.base.utils.HttpMethod
import com.rarnu.base.utils.http
import com.rarnu.tools.neo.data.Onekey
import com.rarnu.tools.neo.data.ThanksInfo
import com.rarnu.tools.neo.data.UpdateInfo
import org.json.JSONObject
import java.util.*

object API {

    val API_BASE = NativeAPI.getBaseURL()
    val DOWNLOAD_URL = API_BASE + "download/"
    val HEAD_URL = API_BASE + "head/"

    fun getUpdateInfo(): UpdateInfo? {
        val jsonStr = http {
            url = API_BASE + "version.php"
            method = HttpMethod.GET
            getParam = "type=last"
        }
        var ret: UpdateInfo? = null
        try {
            val jobj = JSONObject(jsonStr)
            ret = UpdateInfo.fromJson(jobj)
        } catch (e: Exception) {

        }
        return ret
    }

    fun getAllUpdateInfo(): MutableList<UpdateInfo?>? {
        val jsonStr = http {
            url = API_BASE + "version.php"
            method = HttpMethod.GET
            getParam = "type=all"
        }
        var ret: MutableList<UpdateInfo?>? = null
        try {
            val jobj = JSONObject(jsonStr)
            ret = UpdateInfo.listFromJson(jobj)
        } catch (e: Exception) {

        }
        return ret
    }

    fun getOnekey(pkgName: String?, versionCode: Int): Onekey? {
        val str = http {
            url = API_BASE + "onekey.php"
            method = HttpMethod.GET
            getParam = "action=get&pkg=$pkgName&ver=$versionCode"
        }
        var ok: Onekey? = null
        if (str != null && str.trim { it <= ' ' } != "") {
            ok = Onekey(str)
        }
        return ok
    }

    fun uploadOnekey(pkgName: String, versionCode: Int, disabled: List<String?>?): Boolean {
        // upload onekey
        val param = hashMapOf<String, String>()
        param["action"] = "put"
        param["pkg"] = pkgName
        param["ver"] = versionCode.toString()
        var data = ""
        if (disabled != null && disabled.isNotEmpty()) {
            for (s in disabled) {
                data += s + "\n"
            }
        }
        param["data"] = data
        val str = http {
            url = API_BASE + "onekey.php"
            method = HttpMethod.POST
            postParam = param
        }
        return str == "OK"
    }

    fun sendFeedback(nickname: String, comment: String, photo: Array<String>): Boolean {
        val params = hashMapOf<String, String>()
        params["nickname"] = nickname
        params["comment"] = comment
        val files = HashMap<String, String>()
        photo.indices.filter { photo[it] != "" }.forEach { files["photo${it + 1}"] = photo[it] }
        val data = http {
            url = API_BASE + "feedback.php"
            method = HttpMethod.POST
            postParam = params
            fileParam = files
        }
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
        val data = http {
            url = API_BASE + "thanks.php"
            method = HttpMethod.GET
            getParam = ""
        }
        var list: MutableList<ThanksInfo?>? = null
        try {
            val json = JSONObject(data)
            if (json.getInt("result") == 0) {
                list = arrayListOf()
                val arr = json.getJSONArray("data")
                (0 until arr.length()).mapTo(list) {
                    ThanksInfo.fromJson(arr.getJSONObject(it))
                }
            }
        } catch (e: Exception) {

        }
        return list
    }

    fun reportCrash(ctx: Context?, data: String) {
        // report crash
        val param = hashMapOf<String, String>()
        param["model"] = Build.MODEL
        param["sdk"] = Build.VERSION.SDK_INT.toString()
        val info = ctx?.packageManager?.getPackageInfo(ctx.packageName, 0)
        param["appver"] = info?.versionCode.toString()
        param["data"] = data
        http {
            url = API_BASE + "crash.php"
            method = HttpMethod.POST
            postParam = param
        }
    }

}
