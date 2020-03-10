@file:Suppress("DEPRECATION")

package com.rarnu.tools.neo.data

import android.content.Context
import com.rarnu.common.forEach
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
                val verCode = pkg.versionCode
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
                info = json.run { UpdateInfo().apply {
                    versionCode = getInt("versionCode")
                    versionName = getString("versionName")
                    url = getString("url")
                    description = getString("description")
                    descriptionEn = getString("desc_en")
                } }
            } catch (e: Exception) {

            }
            return info
        }

        fun listFromJson(json: JSONObject): List<UpdateInfo>? {
            var list: MutableList<UpdateInfo>? = null
            try {
                if (json.getInt("result") == 0) {
                    val arr = json.getJSONArray("data")
                    list = mutableListOf()
                    arr.forEach { _, obj ->
                        val info = fromJson(obj)
                        if (info != null) list.add(info)
                    }
                }
            } catch (e: Exception) {
            }
            return list
        }
    }

}
