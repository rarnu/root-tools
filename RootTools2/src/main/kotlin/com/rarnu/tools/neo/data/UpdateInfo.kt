package com.rarnu.tools.neo.data

import android.content.Context
import org.json.JSONObject

class UpdateInfo {

    var versionName = ""
    var versionCode = 0
    var description = ""
    var descriptionEn = ""
    var url = ""

    fun isNewVersion(ctx: Context): Boolean {
        var ret = false
        try {
            val pm = ctx.packageManager
            val pkg = pm?.getPackageInfo(ctx.packageName, 0)
            if (pkg != null) {
                val verCode = pkg.longVersionCode
                ret = versionCode > verCode
            }
        } catch (e: Exception) {
        }
        return ret
    }

    companion object {

        fun fromJson(json: JSONObject): UpdateInfo? {
            var info: UpdateInfo? = null
            try {
                info = UpdateInfo()
                info.versionCode = json.getInt("versionCode")
                info.versionName = json.getString("versionName")
                info.url = json.getString("url")
                info.description = json.getString("description")
                info.descriptionEn = json.getString("desc_en")
            } catch (e: Exception) {

            }
            return info
        }

        fun listFromJson(json: JSONObject): MutableList<UpdateInfo>? {
            var list: MutableList<UpdateInfo>? = null
            try {
                if (json.getInt("result") == 0) {
                    val arr = json.getJSONArray("data")
                    list = arrayListOf()
                    (0 until arr.length()).forEach {
                        val info = fromJson(arr.getJSONObject(it))
                        if (info != null) {
                            list.add(info)
                        }
                    }
                }
            } catch (e: Exception) {

            }
            return list
        }
    }

}
