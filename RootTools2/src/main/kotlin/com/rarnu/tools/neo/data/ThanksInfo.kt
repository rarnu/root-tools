package com.rarnu.tools.neo.data

import org.json.JSONObject

/**
 * Created by rarnu on 12/5/16.
 */
data class ThanksInfo(var id: Int, var name: String?, var headFile: String?, var desc: String?) {

    companion object {
        fun fromJson(json: JSONObject): ThanksInfo? {
            var info: ThanksInfo? = null
            try {
                info = ThanksInfo(
                        json.getInt("id"),
                        json.getString("name"),
                        json.getString("head"),
                        json.getString("desc")
                )
            } catch (e: Exception) {

            }
            return info
        }
    }

}