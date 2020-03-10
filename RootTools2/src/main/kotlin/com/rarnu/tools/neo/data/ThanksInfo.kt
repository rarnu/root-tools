package com.rarnu.tools.neo.data

import org.json.JSONObject

/**
 * Created by rarnu on 12/5/16.
 */
data class ThanksInfo(var id: Int, var name: String?, var headFile: String?, var desc: String?, var descEn: String?) {

    companion object {
        fun fromJson(json: JSONObject): ThanksInfo? {
            var info: ThanksInfo? = null
            try {
                info = json.run { ThanksInfo(getInt("id"), getString("name"), getString("head"), getString("desc"), getString("desc_en")) }
            } catch (e: Exception) {
            }
            return info
        }
    }

}