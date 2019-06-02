@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.content.Context
import android.content.SharedPreferences
import android.os.Build
import android.os.Bundle
import android.preference.Preference
import com.rarnu.android.BackPreferenceActivity
import com.rarnu.android.resStr
import com.rarnu.android.toast
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.comp.PreferenceEx
import com.rarnu.tools.neo.utils.AppUtils
import com.rarnu.tools.neo.xposed.XpStatus

/**
 * Created by rarnu on 11/23/16.
 */
class SettingsActivity : BackPreferenceActivity(), Preference.OnPreferenceClickListener {

    private lateinit var pAdChoose: PreferenceEx
    private lateinit var pDeepClean: PreferenceEx
    private lateinit var pShowThemeCrack: PreferenceEx
    private lateinit var pPreventFreezeReverse: PreferenceEx
    private lateinit var pref: SharedPreferences

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        actionBar?.title = resStr(R.string.settings_name)
    }

    override fun getPreferenceXml() = R.xml.settings

    override fun onPreparedPreference() {
        pref = getSharedPreferences(XpStatus.PREF, Context.MODE_PRIVATE)

        pAdChoose = findPref(R.string.id_settings_adchoose)
        pDeepClean = findPref(R.string.id_settings_deep_clean)
        pShowThemeCrack = findPref(R.string.id_settings_show_theme_crack)
        pPreventFreezeReverse = findPref(R.string.id_settings_prevent_freeze_reverser)

        pAdChoose.onPreferenceClickListener = this
        pDeepClean.onPreferenceClickListener = this
        pShowThemeCrack.onPreferenceClickListener = this
        pPreventFreezeReverse.onPreferenceClickListener = this

        pPreventFreezeReverse.isEnabled = XpStatus.isEnable()
        pAdChoose.status = pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)
        pAdChoose.setSummary(if (pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) R.string.settings_adchoose_detail else R.string.settings_adchoose_onekey)
        pDeepClean.status = pref.getBoolean(XpStatus.KEY_DEEP_CLEAN, false)
        pShowThemeCrack.status = pref.getBoolean(XpStatus.KEY_SHOW_THEME_CRACK, false)
        pPreventFreezeReverse.status = pref.getBoolean(XpStatus.KEY_PREVENT_FREEZE_REVERSE, false)

        val isMIUI = AppUtils.isMIUI(this)
        if (!isMIUI) {
            screen().removePreference(pAdChoose)
            screen().removePreference(pShowThemeCrack)
        }
        if (!pref.getBoolean(XpStatus.KEY_SHOW_THEME_CRACK, false)) {
            screen().removePreference(pShowThemeCrack)
        }

    }

    override fun onPreferenceClick(preference: Preference): Boolean {
        val prefKey = preference.key
        val ex = preference as PreferenceEx
        ex.status = !ex.status
        when (prefKey) {
            getString(R.string.id_settings_adchoose) -> {
                pref.edit()
                        .putBoolean(XpStatus.KEY_AD_CHOOSE, ex.status)
                        .putBoolean(XpStatus.KEY_REMOVEAD, false)
                        .putBoolean(XpStatus.KEY_AD_BROWSER, false)
                        .putBoolean(XpStatus.KEY_AD_CALENDAR, false)
                        .putBoolean(XpStatus.KEY_AD_CLEANMASTER, false)
                        .putBoolean(XpStatus.KEY_AD_DOWNLOAD, false)
                        .putBoolean(XpStatus.KEY_AD_FILEEXPLORER, false)
                        .putBoolean(XpStatus.KEY_AD_CONTACTS, false)
                        .putBoolean(XpStatus.KEY_AD_MMS, false)
                        .putBoolean(XpStatus.KEY_AD_SEARCHBOX, false)
                        .putBoolean(XpStatus.KEY_AD_VIDEO, false)
                        .putBoolean(XpStatus.KEY_AD_MUSIC, false)
                        .putBoolean(XpStatus.KEY_AD_WEATHER, false)
                        .putBoolean(XpStatus.KEY_AD_THEMEMANAGER, false)
                        .putBoolean(XpStatus.KEY_AD_MARKET, false)
                        .putBoolean(XpStatus.KEY_AD_SETTINGS, false)
                        .putBoolean(XpStatus.KEY_AD_SYSTEM, false).apply()
                pAdChoose.setSummary(if (pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) R.string.settings_adchoose_detail else R.string.settings_adchoose_onekey)
            }
            getString(R.string.id_settings_deep_clean) -> pref.edit().putBoolean(XpStatus.KEY_DEEP_CLEAN, ex.status).apply()
            getString(R.string.id_settings_show_theme_crack) -> pref.edit().putBoolean(XpStatus.KEY_SHOW_THEME_CRACK, ex.status).apply()
            getString(R.string.id_settings_prevent_freeze_reverser) -> pref.edit().putBoolean(XpStatus.KEY_PREVENT_FREEZE_REVERSE, ex.status).apply()
        }
        DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, packageName)
        return true
    }
}
