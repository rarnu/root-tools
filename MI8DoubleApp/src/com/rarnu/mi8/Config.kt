package com.rarnu.mi8

import android.content.Context
import android.preference.PreferenceManager

/**
 * Created by rarnu on 6/27/16.
 */
object Config {

    val KEY_WATCHING = "key_watching"
    val KEY_WEIXIN = "key_weixin"
    val KEY_QQ = "key_qq"

    fun isWatching(context: Context): Boolean {
        val pref = PreferenceManager.getDefaultSharedPreferences(context)
        return pref.getBoolean(KEY_WATCHING, false)
    }

    fun setWatching(context: Context, value: Boolean) {
        val pref = PreferenceManager.getDefaultSharedPreferences(context)
        pref.edit().putBoolean(KEY_WATCHING, value).commit()
    }

    fun isDoubleWeiXin(context: Context): Boolean {
        val pref = PreferenceManager.getDefaultSharedPreferences(context)
        return pref.getBoolean(KEY_WEIXIN, false)
    }

    fun setDoubleWeiXin(context: Context, value: Boolean) {
        val pref = PreferenceManager.getDefaultSharedPreferences(context)
        pref.edit().putBoolean(KEY_WEIXIN, value).commit()
    }

    fun isDoubleQQ(context: Context): Boolean {
        val pref = PreferenceManager.getDefaultSharedPreferences(context)
        return pref.getBoolean(KEY_QQ, false)
    }

    fun setDoubleQQ(context: Context, value: Boolean) {
        val pref = PreferenceManager.getDefaultSharedPreferences(context)
        pref.edit().putBoolean(KEY_QQ, value).commit()
    }

}