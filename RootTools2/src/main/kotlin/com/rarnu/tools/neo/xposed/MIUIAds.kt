package com.rarnu.tools.neo.xposed


import com.rarnu.tools.neo.xposed.ads.*
import de.robv.android.xposed.IXposedHookInitPackageResources
import de.robv.android.xposed.IXposedHookLoadPackage
import de.robv.android.xposed.XSharedPreferences
import de.robv.android.xposed.callbacks.XC_InitPackageResources
import de.robv.android.xposed.callbacks.XC_LoadPackage

class MIUIAds : IXposedHookLoadPackage, IXposedHookInitPackageResources {


    @Throws(Throwable::class)
    override fun handleLoadPackage(paramLoadPackageParam: XC_LoadPackage.LoadPackageParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (paramLoadPackageParam.packageName == "com.miui.core") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SYSTEM, false)) {
                FuckCore.fuckCore(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.miui.systemAdSolution") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SYSTEM, false)) {
                FuckAdSolution.fuckAdSolution(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.miui.cleanmaster") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)) {
                FuckCleanMaster.fuckCleanMaster(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.miui.video") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_VIDEO, false)) {
                FuckVideo.fuckVideo(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.android.fileexplorer") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_FILEEXPLORER, false)) {
                FuckFileExplorer.fuckFileExplorer(paramLoadPackageParam)
            }
            return
        }
        if (paramLoadPackageParam.packageName == "com.miui.player") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_MUSIC, false)) {
                FuckMusic.fuckMusic(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.android.providers.downloads.ui") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_DOWNLOAD, false)) {
                FuckDownload.fuckDownload(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.miui.weather2") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_WEATHER, false)) {
                FuckWeather.fuckWeather(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.android.quicksearchbox") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SEARCHBOX, false)) {
                FuckSearchBox.fuckSearchBox(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.android.mms") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_MMS, false)) {
                FuckMms.fuckMms(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.android.calendar") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CALENDAR, false)) {
                FuckCalendar.fuckCalendar(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.android.browser") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_BROWSER, false)) {
                FuckBrowser.fuckBrowser(paramLoadPackageParam)
            }
            return
        }

        if (paramLoadPackageParam.packageName == "com.android.thememanager") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_THEMEMANAGER, false)) {
                FuckThemeManager.fuckThemeManager(paramLoadPackageParam)
            }
            return
        }
        if (paramLoadPackageParam.packageName == "com.xiaomi.market") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_MARKET, false)) {
                FuckMarket.fuckMarket(paramLoadPackageParam)
            }
            return
        }
        if (paramLoadPackageParam.packageName == "com.android.settings") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SETTINGS, false)) {
                FuckSettings.fuckSettings(paramLoadPackageParam)
            }
            return
        }
        if (paramLoadPackageParam.packageName == "com.android.contacts") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CONTACTS, false)) {
                FuckContacts.fuckContacts(paramLoadPackageParam)
            }
        }
    }

    @Throws(Throwable::class)
    override fun handleInitPackageResources(initPackageResourcesParam: XC_InitPackageResources.InitPackageResourcesParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (initPackageResourcesParam.packageName == "com.miui.cleanmaster") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)) {
                FuckCleanMaster.fuckResource(initPackageResourcesParam)
            }
            return
        }
    }
}
