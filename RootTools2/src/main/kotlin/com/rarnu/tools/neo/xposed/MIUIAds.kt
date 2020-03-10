package com.rarnu.tools.neo.xposed


import com.rarnu.tools.neo.xposed.ads.*
import com.rarnu.xfunc.XposedPackage
import com.rarnu.xfunc.XposedPkg
import com.rarnu.xfunc.XposedRes
import com.rarnu.xfunc.XposedResource
import de.robv.android.xposed.XSharedPreferences

class MIUIAdsResources: XposedResource() {
    override fun hook(res: XposedRes) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (res.packageName == "com.miui.cleanmaster") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)) {
                FuckCleanMaster.fuckResource(res)
            }
            return
        }
    }
}

class MIUIAdsPackage : XposedPackage() {
    override fun hook(pkg: XposedPkg) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (pkg.packageName == "com.miui.core") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SYSTEM, false)) {
                FuckCore.fuckCore(pkg)
            }
            return
        }

        if (pkg.packageName == "com.miui.systemAdSolution") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SYSTEM, false)) {
                FuckAdSolution.fuckAdSolution(pkg)
            }
            return
        }

        if (pkg.packageName == "com.miui.cleanmaster") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)) {
                FuckCleanMaster.fuckCleanMaster(pkg)
            }
            return
        }

        if (pkg.packageName == "com.miui.securitycenter") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)) {
                FuckCleanMaster.fuckSecurityCenter(pkg)
            }
        }

        if (pkg.packageName == "com.miui.video") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_VIDEO, false)) {
                FuckVideo.fuckVideo(pkg)
            }
            return
        }

        if (pkg.packageName == "com.android.fileexplorer") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_FILEEXPLORER, false)) {
                FuckFileExplorer.fuckFileExplorer(pkg)
            }
            return
        }
        if (pkg.packageName == "com.miui.player") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_MUSIC, false)) {
                FuckMusic.fuckMusic(pkg)
            }
            return
        }

        if (pkg.packageName == "com.android.providers.downloads.ui") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_DOWNLOAD, false)) {
                FuckDownload.fuckDownload(pkg)
            }
            return
        }

        if (pkg.packageName == "com.miui.weather2") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_WEATHER, false)) {
                FuckWeather.fuckWeather(pkg)
            }
            return
        }

        if (pkg.packageName == "com.android.quicksearchbox") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SEARCHBOX, false)) {
                FuckSearchBox.fuckSearchBox(pkg)
            }
            return
        }

        if (pkg.packageName == "com.android.mms") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_MMS, false)) {
                FuckMms.fuckMms(pkg)
            }
            return
        }

        if (pkg.packageName == "com.android.calendar") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CALENDAR, false)) {
                FuckCalendar.fuckCalendar(pkg)
            }
            return
        }

        if (pkg.packageName == "com.android.browser") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_BROWSER, false)) {
                FuckBrowser.fuckBrowser(pkg)
            }
            return
        }

        if (pkg.packageName == "com.android.thememanager") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_THEMEMANAGER, false)) {
                FuckThemeManager.fuckThemeManager(pkg)
            }
            return
        }
        if (pkg.packageName == "com.xiaomi.market") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_MARKET, false)) {
                FuckMarket.fuckMarket(pkg)
            }
            return
        }
        if (pkg.packageName == "com.android.settings") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SETTINGS, false)) {
                FuckSettings.fuckSettings(pkg)
            }
            return
        }
        if (pkg.packageName == "com.android.contacts") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CONTACTS, false)) {
                FuckContacts.fuckContacts(pkg)
            }
            return
        }
        if (pkg.packageName == "com.miui.cloudservice") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CLOUDSERVICE, false)) {
                FuckCloudService.fuckCloudService(pkg)
            }
            return
        }

        if (pkg.packageName == "com.xiaomi.account") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_ACCOUNT, false)) {
                FuckAccount.fuckAccount(pkg)
            }
            return
        }

        if (pkg.packageName == "com.xiaomi.vip") {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_ACCOUNT, false)) {
                FuckAccount.fuckVip(pkg)
            }
        }
    }

}
