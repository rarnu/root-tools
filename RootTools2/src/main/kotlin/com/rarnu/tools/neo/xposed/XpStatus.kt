package com.rarnu.tools.neo.xposed

object XpStatus {

    const val PKGNAME = "com.rarnu.tools.neo"
    const val PREF = "settings"

    const val KEY_REMOVESEARCHBAR = "removesearchbar"
    const val KEY_THEMECRACK = "themecrack"
    const val KEY_REMOVEAD = "removead"
    const val KEY_REMOVEAD_ROOT = "removead_root"
    const val KEY_ROOTCRACK = "rootcrack"
    const val KEY_CORECRACK = "corecrack2"
    const val KEY_NOUPDATE = "noupate"
    const val KEY_MINUS_SCREEN = "minusscreen"

    // settings
    const val KEY_WORK_MODE = "work_mode"
    const val KEY_AD_CHOOSE = "ad_choose"
    const val KEY_DEEP_CLEAN = "deep_clean"
    const val KEY_SHOW_THEME_CRACK = "show_theme_crack"
    const val KEY_PREVENT_FREEZE_REVERSE = "prevent_freeze_reverse"

    // ad fucker for special apps
    const val KEY_AD_WEATHER = "fuck_weather"
    const val KEY_AD_MUSIC = "fuck_music"
    const val KEY_AD_VIDEO = "fuck_video"
    const val KEY_AD_SEARCHBOX = "fuck_searchbox"
    const val KEY_AD_MMS = "fuck_mms"
    const val KEY_AD_FILEEXPLORER = "fuck_fileexplorer"
    const val KEY_AD_DOWNLOAD = "fuck_download"
    const val KEY_AD_CLEANMASTER = "fuck_cleanmaster"
    const val KEY_AD_CALENDAR = "fuck_calendar"
    const val KEY_AD_BROWSER = "fuck_browser"
    const val KEY_AD_THEMEMANAGER = "fuck_thememanager"
    const val KEY_AD_SYSTEM = "fuck_system"
    const val KEY_AD_MARKET = "fuck_market"
    const val KEY_AD_SETTINGS = "fuck_settings"
    const val KEY_AD_CONTACTS = "fuck_contacts"
    const val KEY_AD_CLOUDSERVICE = "fuck_cloudservice"
    const val KEY_AD_ACCOUNT = "fuck_account"

    var canWriteSdcard = false

    fun isEnable(): Boolean = false
}
