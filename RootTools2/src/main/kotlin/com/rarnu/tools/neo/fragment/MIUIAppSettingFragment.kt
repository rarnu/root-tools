package com.rarnu.tools.neo.fragment

import android.content.SharedPreferences
import android.os.Build
import android.os.Bundle
import android.preference.Preference
import android.view.Menu
import com.rarnu.tools.neo.R
import com.rarnu.tools.neo.api.DeviceAPI
import com.rarnu.tools.neo.base.BasePreferenceFragment
import com.rarnu.tools.neo.comp.PreferenceEx
import com.rarnu.tools.neo.xposed.XpStatus

/**
 * Created by rarnu on 11/23/16.
 */
class MIUIAppSettingFragment : BasePreferenceFragment(), Preference.OnPreferenceClickListener {

    private var pAccount: PreferenceEx? = null
    private var pBrowser: PreferenceEx? = null
    private var pCalendar: PreferenceEx? = null
    private var pCleanMaster: PreferenceEx? = null
    private var pDownload: PreferenceEx? = null
    private var pFileExplorer: PreferenceEx? = null
    private var pContact: PreferenceEx? = null
    private var pMms: PreferenceEx? = null
    private var pMusic: PreferenceEx? = null
    private var pSearchBox: PreferenceEx? = null
    private var pVideo: PreferenceEx? = null
    private var pWeather: PreferenceEx? = null
    private var pTheme: PreferenceEx? = null
    private var pMarket: PreferenceEx? = null
    private var pCloudService: PreferenceEx? = null
    private var pSettings: PreferenceEx? = null
    private var pSystem: PreferenceEx? = null
    private var pref: SharedPreferences? = null
    private var editor: SharedPreferences.Editor? = null

    override fun getBarTitle(): Int = R.string.miui_app_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        pref = context?.getSharedPreferences(XpStatus.PREF, if (Build.VERSION.SDK_INT < 24) 1 else 0)
        editor = pref?.edit()
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
    }

    private fun findPref(prefId: Int): PreferenceEx = findPreference(getString(prefId)) as PreferenceEx

    override fun initEvents() {
        pAccount?.onPreferenceClickListener = this
        pBrowser?.onPreferenceClickListener = this
        pCalendar?.onPreferenceClickListener = this
        pCleanMaster?.onPreferenceClickListener = this
        pDownload?.onPreferenceClickListener = this
        pFileExplorer?.onPreferenceClickListener = this
        pContact?.onPreferenceClickListener = this
        pMms?.onPreferenceClickListener = this
        pMusic?.onPreferenceClickListener = this
        pSearchBox?.onPreferenceClickListener = this
        pVideo?.onPreferenceClickListener = this
        pWeather?.onPreferenceClickListener = this
        pTheme?.onPreferenceClickListener = this
        pMarket?.onPreferenceClickListener = this
        pCloudService?.onPreferenceClickListener = this
        pSettings?.onPreferenceClickListener = this
        pSystem?.onPreferenceClickListener = this
    }

    override fun initLogic() {
        pAccount?.status = pref!!.getBoolean(XpStatus.KEY_AD_ACCOUNT, false)
        pBrowser?.status = pref!!.getBoolean(XpStatus.KEY_AD_BROWSER, false)
        pCalendar?.status = pref!!.getBoolean(XpStatus.KEY_AD_CALENDAR, false)
        pCleanMaster?.status = pref!!.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)
        pDownload?.status = pref!!.getBoolean(XpStatus.KEY_AD_DOWNLOAD, false)
        pFileExplorer?.status = pref!!.getBoolean(XpStatus.KEY_AD_FILEEXPLORER, false)
        pContact?.status = pref!!.getBoolean(XpStatus.KEY_AD_CONTACTS, false)
        pMms?.status = pref!!.getBoolean(XpStatus.KEY_AD_MMS, false)
        pMusic?.status = pref!!.getBoolean(XpStatus.KEY_AD_MUSIC, false)
        pSearchBox?.status = pref!!.getBoolean(XpStatus.KEY_AD_SEARCHBOX, false)
        pVideo?.status = pref!!.getBoolean(XpStatus.KEY_AD_VIDEO, false)
        pWeather?.status = pref!!.getBoolean(XpStatus.KEY_AD_WEATHER, false)
        pTheme?.status = pref!!.getBoolean(XpStatus.KEY_AD_THEMEMANAGER, false)
        pMarket?.status = pref!!.getBoolean(XpStatus.KEY_AD_MARKET, false)
        pCloudService?.status = pref!!.getBoolean(XpStatus.KEY_AD_CLOUDSERVICE, false)
        pSettings?.status = pref!!.getBoolean(XpStatus.KEY_AD_SETTINGS, false)
        pSystem?.status = pref!!.getBoolean(XpStatus.KEY_AD_SYSTEM, false)
    }

    override fun getFragmentLayoutResId(): Int = R.xml.miui_apps

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onPreferenceClick(preference: Preference): Boolean {
        val prefKey = preference.key
        val ex = preference as PreferenceEx
        ex.status = !ex.status
        when (prefKey) {
            getString(R.string.id_app_account) -> editor?.putBoolean(XpStatus.KEY_AD_ACCOUNT, ex.status)
            getString(R.string.id_app_browser) -> editor?.putBoolean(XpStatus.KEY_AD_BROWSER, ex.status)
            getString(R.string.id_app_calendar) -> editor?.putBoolean(XpStatus.KEY_AD_CALENDAR, ex.status)
            getString(R.string.id_app_cleanmaster) -> editor?.putBoolean(XpStatus.KEY_AD_CLEANMASTER, ex.status)
            getString(R.string.id_app_download) -> editor?.putBoolean(XpStatus.KEY_AD_DOWNLOAD, ex.status)
            getString(R.string.id_app_fileexplorer) -> editor?.putBoolean(XpStatus.KEY_AD_FILEEXPLORER, ex.status)
            getString(R.string.id_app_contacts) -> editor?.putBoolean(XpStatus.KEY_AD_CONTACTS, ex.status)
            getString(R.string.id_app_mms) -> editor?.putBoolean(XpStatus.KEY_AD_MMS, ex.status)
            getString(R.string.id_app_music) -> editor?.putBoolean(XpStatus.KEY_AD_MUSIC, ex.status)
            getString(R.string.id_app_searchbox) -> editor?.putBoolean(XpStatus.KEY_AD_SEARCHBOX, ex.status)
            getString(R.string.id_app_video) -> editor?.putBoolean(XpStatus.KEY_AD_VIDEO, ex.status)
            getString(R.string.id_app_weather) -> editor?.putBoolean(XpStatus.KEY_AD_WEATHER, ex.status)
            getString(R.string.id_app_thememanager) -> editor?.putBoolean(XpStatus.KEY_AD_THEMEMANAGER, ex.status)
            getString(R.string.id_app_market) -> editor?.putBoolean(XpStatus.KEY_AD_MARKET, ex.status)
            getString(R.string.id_app_cloudservice) -> editor?.putBoolean(XpStatus.KEY_AD_CLOUDSERVICE, ex.status)
            getString(R.string.id_app_settings) -> editor?.putBoolean(XpStatus.KEY_AD_SETTINGS, ex.status)
            getString(R.string.id_app_system) -> editor?.putBoolean(XpStatus.KEY_AD_SYSTEM, ex.status)
        }
        editor?.apply()
        DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, context?.packageName)
        return true
    }
}
