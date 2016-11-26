package com.rarnu.tools.neo.xposed;


import com.rarnu.tools.neo.xposed.ads.*;
import de.robv.android.xposed.IXposedHookInitPackageResources;
import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.XSharedPreferences;
import de.robv.android.xposed.callbacks.XC_InitPackageResources;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

public class MIUIAds implements IXposedHookLoadPackage, IXposedHookInitPackageResources {


    public void handleLoadPackage(XC_LoadPackage.LoadPackageParam paramLoadPackageParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();

        if (paramLoadPackageParam.packageName.equals("com.miui.core")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SYSTEM, false)) {
                FuckCore.fuckCore(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.systemAdSolution")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SYSTEM, false)) {
                FuckAdSolution.fuckAdSolution(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.cleanmaster")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)) {
                FuckCleanMaster.fuckCleanMaster(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.video")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_VIDEO, false)) {
                FuckVideo.fuckVideo(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.fileexplorer")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_FILEEXPLORER, false)) {
                FuckFileExplorer.fuckFileExplorer(paramLoadPackageParam);
            }
            return;
        }
        if (paramLoadPackageParam.packageName.equals("com.miui.player")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_MUSIC, false)) {
                FuckMusic.fuckMusic(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.providers.downloads.ui")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_DOWNLOAD, false)) {
                FuckDownload.fuckDownload(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.weather2")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_WEATHER, false)) {
                FuckWeather.fuckWeather(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.quicksearchbox")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_SEARCHBOX, false)) {
                FuckSearchBox.fuckSearchBox(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.mms")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_MMS, false)) {
                FuckMms.fuckMms(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.calendar")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CALENDAR, false)) {
                FuckCalendar.fuckCalendar(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.browser")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_BROWSER, false)) {
                FuckBrowser.fuckBrowser(paramLoadPackageParam);
            }
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.thememanager")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_THEMEMANAGER, false)) {
                FuckThemeManager.fuckThemeManager(paramLoadPackageParam);
            }
        }
    }

    @Override
    public void handleInitPackageResources(XC_InitPackageResources.InitPackageResourcesParam initPackageResourcesParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (initPackageResourcesParam.packageName.equals("com.miui.cleanmaster")) {
            if (prefs.getBoolean(XpStatus.KEY_REMOVEAD, false) || prefs.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false)) {
                FuckCleanMaster.fuckResource(initPackageResourcesParam);
            }
            return;
        }
    }
}
