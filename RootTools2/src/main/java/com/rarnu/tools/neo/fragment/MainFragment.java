package com.rarnu.tools.neo.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.preference.Preference;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.activity.*;
import com.rarnu.tools.neo.api.API;
import com.rarnu.tools.neo.base.BasePreferenceFragment;
import com.rarnu.tools.neo.comp.PreferenceEx;
import com.rarnu.tools.neo.data.UpdateInfo;
import com.rarnu.tools.neo.root.RootUtils;
import com.rarnu.tools.neo.utils.FileUtils;
import com.rarnu.tools.neo.utils.HostsUtils;
import com.rarnu.tools.neo.xposed.XpStatus;

public class MainFragment extends BasePreferenceFragment implements Preference.OnPreferenceClickListener, UpdateInfo.UpdateInfoReadyCallback {

    // system
    private PreferenceEx pFreeze, pComponent, pCleanArt, pCoreCrack, pFakeDevice, pTerminal;
    // miui
    private PreferenceEx pTheme, pRemoveAd, pRemoveSearch, pColumns, pRoot25, pNoUpdate;
    // about
    private PreferenceEx pAbout;

    // pref
    private SharedPreferences pref = null;
    private SharedPreferences.Editor editor = null;

    //
    private MenuItem miShare = null;

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
        pref = getContext().getSharedPreferences(XpStatus.PREF, 1);
        editor = pref.edit();

        // system
        pFreeze = findPref(R.string.id_freeze);
        pComponent = findPref(R.string.id_component);
        pCleanArt = findPref(R.string.id_cleanart);
        pCoreCrack = findPref(R.string.id_corecrack);
        pFakeDevice = findPref(R.string.id_fake_device);
        pTerminal = findPref(R.string.id_terminal);

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
        pTerminal.setOnPreferenceClickListener(this);

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
        UpdateInfo.getUpdateInfo(this);
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
        menu.clear();
        miShare = menu.add(0, 1, 1, R.string.ab_share);
        miShare.setIcon(android.R.drawable.ic_menu_help);
        miShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case 1:
                showGPLLicense();
                break;
        }
        return true;
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
    }

    private void setXposedRootStatus() {
        // system
        pFreeze.setEnabled(!RootUtils.isRejected());
        pComponent.setEnabled(!RootUtils.isRejected());
        pCleanArt.setEnabled(!RootUtils.isRejected());
        pCoreCrack.setEnabled(XpStatus.isEnable());
        pFakeDevice.setEnabled(!RootUtils.isRejected());
        pTerminal.setEnabled(true);
        // miui
        pTheme.setEnabled(XpStatus.isEnable());
        pRemoveAd.setEnabled(XpStatus.isEnable() && !RootUtils.isRejected());
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

    private void threadWriteHost() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                HostsUtils.writeHost(getContext(), pref.getBoolean(XpStatus.KEY_NOUPDATE, false), pref.getBoolean(XpStatus.KEY_REMOVEAD, false));
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
            editor.putBoolean(XpStatus.KEY_THEMECRACK, ex.getStatus()).apply();
        } else if (prefKey.equals(getString(R.string.id_removead))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_REMOVEAD, ex.getStatus()).apply();
            threadWriteHost();
        } else if (prefKey.equals(getString(R.string.id_removesearch))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_REMOVESEARCHBAR, ex.getStatus()).apply();
        } else if (prefKey.equals(getString(R.string.id_root25))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_ROOTCRACK, ex.getStatus()).apply();
        } else if (prefKey.equals(getString(R.string.id_corecrack))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_CORECRACK, ex.getStatus()).apply();
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
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_NOUPDATE, ex.getStatus()).apply();
            threadWriteHost();
        }
        return true;
    }

    @Override
    public void onUpdateInfoReady(final UpdateInfo info) {
        if (info.isNewVersion(getContext())) {
            new AlertDialog.Builder(getContext())
                    .setTitle(R.string.alert_hint)
                    .setMessage(getString(R.string.alert_update_message, info.versionName, info.versionCode, info.description))
                    .setPositiveButton(R.string.alert_update, new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            downloadApk(info.url);
                        }
                    })
                    .setNegativeButton(R.string.alert_cancel, null)
                    .show();
        }
    }

    private void downloadApk(String url) {
        String http = API.DOWNLOAD_URL + url;
        Intent inDownload = new Intent(Intent.ACTION_VIEW);
        inDownload.setData(Uri.parse(http));
        startActivity(inDownload);
    }

    private void showGPLLicense() {
        Intent inGPL = new Intent(Intent.ACTION_VIEW);
        inGPL.setData(Uri.parse(getString(R.string.view_gpl_license_url)));
        startActivity(inGPL);
    }
}
