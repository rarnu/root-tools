package com.rarnu.tools.neo.fragment;

import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.preference.Preference;
import android.view.Menu;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.api.DeviceAPI;
import com.rarnu.tools.neo.base.BasePreferenceFragment;
import com.rarnu.tools.neo.comp.PreferenceEx;
import com.rarnu.tools.neo.xposed.XpStatus;

/**
 * Created by rarnu on 11/23/16.
 */
public class MIUIAppSettingFragment extends BasePreferenceFragment implements Preference.OnPreferenceClickListener {

    private PreferenceEx pBrowser, pCalendar, pCleanMaster, pDownload, pFileExplorer, pMms, pMusic, pSearchBox, pVideo, pWeather, pSystem;
    private SharedPreferences pref = null;
    private SharedPreferences.Editor editor = null;

    @Override
    public int getBarTitle() {
        return R.string.miui_app_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        pref = getContext().getSharedPreferences(XpStatus.PREF, Build.VERSION.SDK_INT < 24 ? 1 : 0);
        editor = pref.edit();
        pBrowser = findPref(R.string.id_app_browser);
        pCalendar = findPref(R.string.id_app_calendar);
        pCleanMaster = findPref(R.string.id_app_cleanmaster);
        pDownload = findPref(R.string.id_app_download);
        pFileExplorer = findPref(R.string.id_app_fileexplorer);
        pMms = findPref(R.string.id_app_mms);
        pMusic = findPref(R.string.id_app_music);
        pSearchBox = findPref(R.string.id_app_searchbox);
        pVideo = findPref(R.string.id_app_video);
        pWeather = findPref(R.string.id_app_weather);
        pSystem = findPref(R.string.id_app_system);
    }

    private PreferenceEx findPref(int prefId) {
        return (PreferenceEx) findPreference(getString(prefId));
    }

    @Override
    public void initEvents() {
        pBrowser.setOnPreferenceClickListener(this);
        pCalendar.setOnPreferenceClickListener(this);
        pCleanMaster.setOnPreferenceClickListener(this);
        pDownload.setOnPreferenceClickListener(this);
        pFileExplorer.setOnPreferenceClickListener(this);
        pMms.setOnPreferenceClickListener(this);
        pMusic.setOnPreferenceClickListener(this);
        pSearchBox.setOnPreferenceClickListener(this);
        pVideo.setOnPreferenceClickListener(this);
        pWeather.setOnPreferenceClickListener(this);
        pSystem.setOnPreferenceClickListener(this);
    }

    @Override
    public void initLogic() {
        pBrowser.setStatus(pref.getBoolean(XpStatus.KEY_AD_BROWSER, false));
        pCalendar.setStatus(pref.getBoolean(XpStatus.KEY_AD_CALENDAR, false));
        pCleanMaster.setStatus(pref.getBoolean(XpStatus.KEY_AD_CLEANMASTER, false));
        pDownload.setStatus(pref.getBoolean(XpStatus.KEY_AD_DOWNLOAD, false));
        pFileExplorer.setStatus(pref.getBoolean(XpStatus.KEY_AD_FILEEXPLORER, false));
        pMms.setStatus(pref.getBoolean(XpStatus.KEY_AD_MMS, false));
        pMusic.setStatus(pref.getBoolean(XpStatus.KEY_AD_MUSIC, false));
        pSearchBox.setStatus(pref.getBoolean(XpStatus.KEY_AD_SEARCHBOX, false));
        pVideo.setStatus(pref.getBoolean(XpStatus.KEY_AD_VIDEO, false));
        pWeather.setStatus(pref.getBoolean(XpStatus.KEY_AD_WEATHER, false));
        pSystem.setStatus(pref.getBoolean(XpStatus.KEY_AD_SYSTEM, false));
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.xml.miui_apps;
    }

    @Override
    public String getMainActivityName() {
        return null;
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public boolean onPreferenceClick(Preference preference) {
        String prefKey = preference.getKey();
        PreferenceEx ex = (PreferenceEx) preference;
        if (prefKey.equals(getString(R.string.id_app_browser))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_BROWSER, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_calendar))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_CALENDAR, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_cleanmaster))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_CLEANMASTER, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_download))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_DOWNLOAD, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_fileexplorer))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_FILEEXPLORER, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_mms))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_MMS, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_music))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_MUSIC, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_searchbox))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_SEARCHBOX, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_video))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_VIDEO, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_weather))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_WEATHER, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_app_system))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_SYSTEM, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        }
        return true;
    }
}
