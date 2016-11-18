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
        if (!prefs.getBoolean(XpStatus.KEY_REMOVEAD, false)) {
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.core")) {
            FuckCore.fuckCore(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.cleanmaster")) {
            FuckCleanMaster.fuckCleanMaster(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.video")) {
            FuckVideo.fuckVideo(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.fileexplorer")) {
            FuckFileExplorer.fuckFileExplorer(paramLoadPackageParam);
            return;
        }
        if (paramLoadPackageParam.packageName.equals("com.miui.player")) {
            FuckMusic.fuckMusic(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.providers.downloads.ui")) {
            FuckDownload.fuckDownload(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.weather2")) {
            FuckWeather.fuckWeather(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.quicksearchbox")) {
            FuckSearchBox.fuckSearchBox(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.mms")) {
            FuckMms.fuckMms(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.android.calendar")) {
            FuckCalendar.fuckCalendar(paramLoadPackageParam);
            return;
        }

        if (paramLoadPackageParam.packageName.equals("com.miui.systemAdSolution")) {
            FuckAdSolution.fuckAdSolution(paramLoadPackageParam);
            return;
        }
        if (paramLoadPackageParam.packageName.equals("com.android.browser")) {
            FuckBrowser.fuckBrowser(paramLoadPackageParam);
            return;
        }
    }

    @Override
    public void handleInitPackageResources(XC_InitPackageResources.InitPackageResourcesParam initPackageResourcesParam) throws Throwable {
        XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        prefs.reload();
        if (!prefs.getBoolean(XpStatus.KEY_REMOVEAD, false)) {
            return;
        }
        if (initPackageResourcesParam.packageName.equals("com.miui.cleanmaster")) {
            FuckCleanMaster.fuckResource(initPackageResourcesParam);
            return;
        }
    }
}
