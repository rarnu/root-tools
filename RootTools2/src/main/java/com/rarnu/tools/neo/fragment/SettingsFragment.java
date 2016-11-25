package com.rarnu.tools.neo.fragment;

import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.preference.Preference;
import android.view.Menu;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.api.DeviceAPI;
import com.rarnu.tools.neo.base.BasePreferenceFragment;
import com.rarnu.tools.neo.comp.PreferenceEx;
import com.rarnu.tools.neo.utils.AppUtils;
import com.rarnu.tools.neo.xposed.XpStatus;

/**
 * Created by rarnu on 11/23/16.
 */
public class SettingsFragment extends BasePreferenceFragment implements Preference.OnPreferenceClickListener {

    private PreferenceEx pMode, pAdChoose, pDeepClean;
    private SharedPreferences pref = null;
    private SharedPreferences.Editor editor = null;

    @Override
    public int getBarTitle() {
        return R.string.settings_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        pref = getContext().getSharedPreferences(XpStatus.PREF, Build.VERSION.SDK_INT < 24 ? 1 : 0);
        editor = pref.edit();
        pMode = findPref(R.string.id_settings_mode);
        pAdChoose = findPref(R.string.id_settings_adchoose);
        pDeepClean = findPref(R.string.id_settings_deep_clean);
    }

    private PreferenceEx findPref(int prefId) {
        return (PreferenceEx) findPreference(getString(prefId));
    }

    @Override
    public void initEvents() {
        pMode.setOnPreferenceClickListener(this);
        pAdChoose.setOnPreferenceClickListener(this);
        pDeepClean.setOnPreferenceClickListener(this);
    }

    @Override
    public void initLogic() {
        pMode.setStatus(pref.getBoolean(XpStatus.KEY_WORK_MODE, false));
        pMode.setSummary(pref.getBoolean(XpStatus.KEY_WORK_MODE, false) ? R.string.settings_mode_effect : R.string.settings_mode_common);
        pAdChoose.setStatus(pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false));
        pAdChoose.setSummary(pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false) ? R.string.settings_adchoose_detail : R.string.settings_adchoose_onekey);
        pDeepClean.setStatus(pref.getBoolean(XpStatus.KEY_DEEP_CLEAN, false));

        boolean isMIUI = AppUtils.isMIUI(getContext());
        if (!isMIUI) {
            getPreferenceScreen().removePreference(pAdChoose);
        }
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.xml.settings;
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
        if (prefKey.equals(getString(R.string.id_settings_mode))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_WORK_MODE, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
            pMode.setSummary(pref.getBoolean(XpStatus.KEY_WORK_MODE, false) ? R.string.settings_mode_effect : R.string.settings_mode_common);
            Toast.makeText(getContext(), R.string.toast_reboot_app, Toast.LENGTH_LONG).show();
        } else if (prefKey.equals(getString(R.string.id_settings_adchoose))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_AD_CHOOSE, ex.getStatus()).apply();
            editor.putBoolean(XpStatus.KEY_REMOVEAD, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_BROWSER, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_CALENDAR, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_CLEANMASTER, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_DOWNLOAD, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_FILEEXPLORER, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_MMS, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_SEARCHBOX, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_VIDEO, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_MUSIC, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_WEATHER, false).apply();
            editor.putBoolean(XpStatus.KEY_AD_SYSTEM, false).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
            pAdChoose.setSummary(pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false) ? R.string.settings_adchoose_detail : R.string.settings_adchoose_onekey);
        } else if (prefKey.equals(getString(R.string.id_settings_deep_clean))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_DEEP_CLEAN, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        }
        return true;
    }
}
