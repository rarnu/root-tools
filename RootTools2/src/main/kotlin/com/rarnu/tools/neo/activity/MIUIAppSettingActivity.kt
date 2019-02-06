@file:Suppress("Duplicates")

package com.rarnu.tools.neo.activity

import android.content.Context
import android.content.SharedPreferences
import android.os.Build
import android.os.Bundle
import android.preference.Preference
import com.rarnu.kt.android.BackPreferenceActivity
import com.rarnu.kt.android.resStr
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.comp.PreferenceEx
import com.rarnu.tools.neo.xposed.XpStatus

/**
 * Created by rarnu on 11/23/16.
 */
class MIUIAppSettingActivity : BackPreferenceActivity(), Preference.OnPreferenceClickListener {

    private lateinit var pAccount: PreferenceEx
    private lateinit var pBrowser: PreferenceEx
    private lateinit var pCalendar: PreferenceEx
    private lateinit var pCleanMaster: PreferenceEx
    private lateinit var pDownload: PreferenceEx
    private lateinit var pFileExplorer: PreferenceEx
    private lateinit var pContact: PreferenceEx
    private lateinit var pMms: PreferenceEx
    private lateinit var pMusic: PreferenceEx
    private lateinit var pSearchBox: PreferenceEx
    private lateinit var pVideo: PreferenceEx
    private lateinit var pWeather: PreferenceEx
    private lateinit var pTheme: PreferenceEx
    private lateinit var pMarket: PreferenceEx
    private lateinit var pCloudService: PreferenceEx
    private lateinit var pSettings: PreferenceEx
    private lateinit var pSystem: PreferenceEx
    private lateinit var pref: SharedPreferences

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        actionBar?.title = resStr(R.string.miui_app_name)
    }

    override fun getPreferenceXml() = R.xml.miui_apps

    override fun onPreparedPreference() {
        pref = getSharedPreferences(XpStatus.PREF, Context.MODE_PRIVATE)

        pAccount = findPref(R.string.id_app_account)
        pBrowser = findPref(R.string.id_app_browser)
        pCalendar = findPref(R.string.id_app_calendar)
        pCleanMaster = findPref(R.string.id_app_cleanmaster)
        pDownload = findPref(R.string.id_app_download)
        pFileExplorer = findPref(R.string.id_app_fileexplorer)
        pContact = findPref(R.string.id_app_contacts)
        pMms = findPref(R.string.id_app_mms)
        pMusic = findPref(R.string.id_app_music)
        pSearchBox = findPref(R.string.id_app_searchbox)
        pVideo = findPref(R.string.id_app_video)
        pWeather = findPref(R.string.id_app_weather)
        pTheme = findPref(R.string.id_app_thememanager)
        pMarket = findPref(R.string.id_app_market)
        pCloudService = findPref(R.string.id_app_cloudservice)
        pSettings = findPref(R.string.id_app_settings)
        pSystem = findPref(R.string.id_app_system)

        pAccount.onPreferenceClickListener = this
        pBrowser.onPreferenceClickListener = this
        pCalendar.onPreferenceClickListener = this
        pCleanMaster.onPreferenceClickListener = this
        pDownload.onPreferenceClickListener = this
        pFileExplorer.onPreferenceClickListener = this
        pContact.onPreferenceClickListener = this
        pMms.onPreferenceClickListener = this
        pMusic.onPreferenceClickListener = this
        pSearchBox.onPreferenceClickListener = this
        pVideo.onPreferenceClickListener = this
        pWeather.onPreferenceClickListener = this
        pTheme.onPreferenceClickListener = this
        pMarket.onPreferenceClickListener = this
        pCloudService.onPreferenceClickListener = this
        pSettings.onPreferenceClickListener = this
        pSystem.onPreferenceClickListener = this

        pAccount.status = pref.getBoolean(XpStatus.KEY_AD_ACCOUNT, false)
        pBrowser.status = pref.getBoolean(XpStatus.KEY_AD_BROWSER, false)
        pCalendar.status = pref.getBoolean(XpStatus.KEY_AD_CALENDAR, false)
        pCleanMaster.status = pref.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)
        pDownload.status = pref.getBoolean(XpStatus.KEY_AD_DOWNLOAD, false)
        pFileExplorer.status = pref.getBoolean(XpStatus.KEY_AD_FILEEXPLORER, false)
        pContact.status = pref.getBoolean(XpStatus.KEY_AD_CONTACTS, false)
        pMms.status = pref.getBoolean(XpStatus.KEY_AD_MMS, false)
        pMusic.status = pref.getBoolean(XpStatus.KEY_AD_MUSIC, false)
        pSearchBox.status = pref.getBoolean(XpStatus.KEY_AD_SEARCHBOX, false)
        pVideo.status = pref.getBoolean(XpStatus.KEY_AD_VIDEO, false)
        pWeather.status = pref.getBoolean(XpStatus.KEY_AD_WEATHER, false)
        pTheme.status = pref.getBoolean(XpStatus.KEY_AD_THEMEMANAGER, false)
        pMarket.status = pref.getBoolean(XpStatus.KEY_AD_MARKET, false)
        pCloudService.status = pref.getBoolean(XpStatus.KEY_AD_CLOUDSERVICE, false)
        pSettings.status = pref.getBoolean(XpStatus.KEY_AD_SETTINGS, false)
        pSystem.status = pref.getBoolean(XpStatus.KEY_AD_SYSTEM, false)

    }

    override fun onPreferenceClick(preference: Preference): Boolean {
        val prefKey = preference.key
        val ex = preference as PreferenceEx
        ex.status = !ex.status
        val editor = pref.edit()
        when (prefKey) {
            getString(R.string.id_app_account) -> editor.putBoolean(XpStatus.KEY_AD_ACCOUNT, ex.status)
            getString(R.string.id_app_browser) -> editor.putBoolean(XpStatus.KEY_AD_BROWSER, ex.status)
            getString(R.string.id_app_calendar) -> editor.putBoolean(XpStatus.KEY_AD_CALENDAR, ex.status)
            getString(R.string.id_app_cleanmaster) -> editor.putBoolean(XpStatus.KEY_AD_CLEANMASTER, ex.status)
            getString(R.string.id_app_download) -> editor.putBoolean(XpStatus.KEY_AD_DOWNLOAD, ex.status)
            getString(R.string.id_app_fileexplorer) -> editor.putBoolean(XpStatus.KEY_AD_FILEEXPLORER, ex.status)
            getString(R.string.id_app_contacts) -> editor.putBoolean(XpStatus.KEY_AD_CONTACTS, ex.status)
            getString(R.string.id_app_mms) -> editor.putBoolean(XpStatus.KEY_AD_MMS, ex.status)
            getString(R.string.id_app_music) -> editor.putBoolean(XpStatus.KEY_AD_MUSIC, ex.status)
            getString(R.string.id_app_searchbox) -> editor.putBoolean(XpStatus.KEY_AD_SEARCHBOX, ex.status)
            getString(R.string.id_app_video) -> editor.putBoolean(XpStatus.KEY_AD_VIDEO, ex.status)
            getString(R.string.id_app_weather) -> editor.putBoolean(XpStatus.KEY_AD_WEATHER, ex.status)
            getString(R.string.id_app_thememanager) -> editor.putBoolean(XpStatus.KEY_AD_THEMEMANAGER, ex.status)
            getString(R.string.id_app_market) -> editor.putBoolean(XpStatus.KEY_AD_MARKET, ex.status)
            getString(R.string.id_app_cloudservice) -> editor.putBoolean(XpStatus.KEY_AD_CLOUDSERVICE, ex.status)
            getString(R.string.id_app_settings) -> editor.putBoolean(XpStatus.KEY_AD_SETTINGS, ex.status)
            getString(R.string.id_app_system) -> editor.putBoolean(XpStatus.KEY_AD_SYSTEM, ex.status)
        }
        editor.apply()
        DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, packageName)
        return true
    }

}
