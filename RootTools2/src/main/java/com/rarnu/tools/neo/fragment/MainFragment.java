package com.rarnu.tools.neo.fragment;

import android.app.AlertDialog;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Message;
import android.preference.Preference;
import android.view.Menu;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.activity.*;
import com.rarnu.tools.neo.base.BasePreferenceFragment;
import com.rarnu.tools.neo.comp.PreferenceEx;
import com.rarnu.tools.neo.root.RootUtils;
import com.rarnu.tools.neo.utils.FileUtils;
import com.rarnu.tools.neo.utils.HostsUtils;
import com.rarnu.tools.neo.xposed.XpStatus;

public class MainFragment extends BasePreferenceFragment implements Preference.OnPreferenceClickListener {

    // system
    private PreferenceEx pFreeze, pComponent, pCleanArt, pCoreCrack, pFakeDevice, pTerminalo;
    // miui
    private PreferenceEx pTheme, pRemoveAd, pRemoveSearch, pColumns, pRoot25, pNoUpdate;
    // about
    private PreferenceEx pAbout;

    // pref
    private SharedPreferences pref = null;
    private SharedPreferences.Editor editor = null;

    @Override
    public int getBarTitle() {
        return R.string.app_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        pref = getActivity().getSharedPreferences(XpStatus.PREF, 1);
        editor = pref.edit();

        // system
        pFreeze = findPref(R.string.id_freeze);
        pComponent = findPref(R.string.id_component);
        pCleanArt = findPref(R.string.id_cleanart);
        pCoreCrack = findPref(R.string.id_corecrack);
        pFakeDevice = findPref(R.string.id_fake_device);
        pTerminalo = findPref(R.string.id_terminal);

        // miui
        pTheme = findPref(R.string.id_theme);
        pRemoveAd = findPref(R.string.id_removead);
        pRemoveSearch = findPref(R.string.id_removesearch);
        pColumns = findPref(R.string.id_columns);
        pRoot25 = findPref(R.string.id_root25);
        pNoUpdate = findPref(R.string.id_noupdate);

        // about
        pAbout = findPref(R.string.id_about);
    }

    private PreferenceEx findPref(int prefId) {
        return (PreferenceEx) findPreference(getString(prefId));
    }

    @Override
    public void initEvents() {
        // system
        pFreeze.setOnPreferenceClickListener(this);
        pComponent.setOnPreferenceClickListener(this);
        pCleanArt.setOnPreferenceClickListener(this);
        pCoreCrack.setOnPreferenceClickListener(this);
        pFakeDevice.setOnPreferenceClickListener(this);
        pTerminalo.setOnPreferenceClickListener(this);

        // miui
        pTheme.setOnPreferenceClickListener(this);
        pRemoveAd.setOnPreferenceClickListener(this);
        pRemoveSearch.setOnPreferenceClickListener(this);
        pColumns.setOnPreferenceClickListener(this);
        pRoot25.setOnPreferenceClickListener(this);
        pNoUpdate.setOnPreferenceClickListener(this);

        // about
        pAbout.setOnPreferenceClickListener(this);
    }

    @Override
    public void initLogic() {
        loadSettings();
        setXposedRootStatus();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.xml.main;
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

    private void loadSettings() {
        pTheme.setStatus(pref.getBoolean(XpStatus.KEY_THEMECRACK, false));
        pRemoveAd.setStatus(pref.getBoolean(XpStatus.KEY_REMOVEAD, false));
        pRemoveSearch.setStatus(pref.getBoolean(XpStatus.KEY_REMOVESEARCHBAR, false));
        pRoot25.setStatus(pref.getBoolean(XpStatus.KEY_ROOTCRACK, false));
        pCoreCrack.setStatus(pref.getBoolean(XpStatus.KEY_CORECRACK, false));
        pNoUpdate.setStatus(pref.getBoolean(XpStatus.KEY_NOUPDATE, false));
        pFakeDevice.setStatus(pref.getBoolean(XpStatus.KEY_FAKE_DEVICE, false));
        pFakeDevice.setSummary(pref.getString(XpStatus.KEY_FAKE_DEVICE_NAME, ""));
    }

    private void setXposedRootStatus() {
        // system
        pFreeze.setEnabled(!RootUtils.isRejected());
        pComponent.setEnabled(!RootUtils.isRejected());
        pCleanArt.setEnabled(!RootUtils.isRejected());
        pCoreCrack.setEnabled(XpStatus.isEnable());
        pFakeDevice.setEnabled(!RootUtils.isRejected());
        pTerminalo.setEnabled(true);
        // miui
        pTheme.setEnabled(XpStatus.isEnable());
        pRemoveAd.setEnabled(XpStatus.isEnable());
        pRemoveSearch.setEnabled(XpStatus.isEnable());
        pColumns.setEnabled(true);
        pRoot25.setEnabled(XpStatus.isEnable());
        pNoUpdate.setEnabled(!RootUtils.isRejected());
        // about
        pAbout.setEnabled(true);
    }

    private void showActivity(Class<?> cls) {
        Intent inA = new Intent(getContext(), cls);
        startActivity(inA);
    }

    private Handler hNoUpdate = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                pNoUpdate.setStatus(msg.arg1 == 1);
                editor.putBoolean(XpStatus.KEY_NOUPDATE, msg.arg1 == 1);
                editor.apply();
            } else {
                Toast.makeText(getContext(), R.string.toast_noupdate_fail, Toast.LENGTH_SHORT).show();
            }
            super.handleMessage(msg);
        }
    };

    private void threadNoUpdate(final boolean oriStatus) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean ret;
                boolean oriTmp = oriStatus;
                boolean newStat = !oriStatus;
                if (newStat) {
                    ret = HostsUtils.writeNoUpdate();
                } else {
                    ret = HostsUtils.restoreHosts();
                }
                if (!ret) {
                    newStat = oriTmp;
                }
                Message msg = new Message();
                msg.what = ret ? 1 : 0;
                msg.arg1 = newStat ? 1 : 0;
                hNoUpdate.sendMessage(msg);
            }
        }).start();
    }

    @Override
    public boolean onPreferenceClick(Preference preference) {
        String prefKey = preference.getKey();
        PreferenceEx ex = (PreferenceEx) preference;
        if (prefKey.equals(getString(R.string.id_freeze))) {
            showActivity(FreezeActivity.class);
        } else if (prefKey.equals(getString(R.string.id_component))) {
            showActivity(ComponentActivity.class);
        } else if (prefKey.equals(getString(R.string.id_terminal))) {
            showActivity(TerminalActivity.class);
        } else if (prefKey.equals(getString(R.string.id_cleanart))) {
            showActivity(CleanActivity.class);
        } else if (prefKey.equals(getString(R.string.id_about))) {
            showActivity(AboutActivity.class);
        } else if (prefKey.equals(getString(R.string.id_fake_device))) {
            showActivity(FakeDeviceActivity.class);
        } else if (prefKey.equals(getString(R.string.id_theme))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_THEMECRACK, ex.getStatus());
            editor.apply();
        } else if (prefKey.equals(getString(R.string.id_removead))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_REMOVEAD, ex.getStatus());
            editor.apply();
        } else if (prefKey.equals(getString(R.string.id_removesearch))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_REMOVESEARCHBAR, ex.getStatus());
            editor.apply();
        } else if (prefKey.equals(getString(R.string.id_root25))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_ROOTCRACK, ex.getStatus());
            editor.apply();
        } else if (prefKey.equals(getString(R.string.id_corecrack))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_CORECRACK, ex.getStatus());
            editor.apply();
        } else if (prefKey.equals(getString(R.string.id_columns))) {
            if (XpStatus.canWriteSdcard) {
                boolean ret = FileUtils.copyAssetFile(getContext(), "RootToolsNeo.mtz", Environment.getExternalStorageDirectory().getAbsolutePath());
                if (ret) {
                    new AlertDialog.Builder(getContext())
                            .setTitle(R.string.alert_hint)
                            .setMessage(R.string.alert_apply_theme)
                            .setPositiveButton(R.string.alert_ok, null)
                            .show();
                } else {
                    Toast.makeText(getContext(), R.string.toast_copyasset_fail, Toast.LENGTH_SHORT).show();
                }
            } else {
                Toast.makeText(getContext(), R.string.toast_no_write_permission, Toast.LENGTH_SHORT).show();
            }
        } else if (prefKey.equals(getString(R.string.id_noupdate))) {
            boolean oriStat = ex.getStatus();
            threadNoUpdate(oriStat);
        }
        return true;
    }
}
