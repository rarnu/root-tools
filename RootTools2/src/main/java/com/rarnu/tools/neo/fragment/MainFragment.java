package com.rarnu.tools.neo.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.preference.Preference;
import android.preference.PreferenceCategory;
import android.view.Menu;
import android.view.MenuItem;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.activity.*;
import com.rarnu.tools.neo.api.API;
import com.rarnu.tools.neo.api.DeviceAPI;
import com.rarnu.tools.neo.base.BasePreferenceFragment;
import com.rarnu.tools.neo.comp.PreferenceEx;
import com.rarnu.tools.neo.data.UpdateInfo;
import com.rarnu.tools.neo.utils.AppUtils;
import com.rarnu.tools.neo.utils.HostsUtils;
import com.rarnu.tools.neo.utils.UIUtils;
import com.rarnu.tools.neo.xposed.XpStatus;

public class MainFragment extends BasePreferenceFragment implements Preference.OnPreferenceClickListener, UpdateInfo.UpdateInfoReadyCallback {

    // categories
    private PreferenceCategory catMain, catMiui, catAbout;

    // system
    private PreferenceEx pFreeze, pComponent, pCleanArt, pCoreCrack, pFakeDevice, pTerminal, pMemory;
    // miui
    private PreferenceEx pTheme, pRemoveAd, pRemoveSearch, pMinusScreen, pKeepMtz, pRoot25, pNoUpdate;
    // about
    private PreferenceEx pFeedback, pAbout;


    // pref
    private SharedPreferences pref = null;
    private SharedPreferences.Editor editor = null;

    //
    private MenuItem miShare = null;
    private MenuItem miSettings = null;

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

        pref = getContext().getSharedPreferences(XpStatus.PREF, Build.VERSION.SDK_INT < 24 ? 1 : 0);
        editor = pref.edit();

        // MODE_WORLD_READABLE is removed on Android N!!!!!
        // categories
        catMain = (PreferenceCategory) findPreference(getString(R.string.catid_system));
        catMiui = (PreferenceCategory) findPreference(getString(R.string.catid_miui));
        catAbout = (PreferenceCategory) findPreference(getString(R.string.catid_about));

        // system
        pFreeze = findPref(R.string.id_freeze);
        pComponent = findPref(R.string.id_component);
        pCleanArt = findPref(R.string.id_cleanart);
        pCoreCrack = findPref(R.string.id_corecrack);
        pFakeDevice = findPref(R.string.id_fake_device);
        pTerminal = findPref(R.string.id_terminal);
        pMemory = findPref(R.string.id_memory);

        // miui
        pTheme = findPref(R.string.id_theme);
        pRemoveAd = findPref(R.string.id_removead);
        pRemoveSearch = findPref(R.string.id_removesearch);
        pMinusScreen = findPref(R.string.id_minus_screen);
        pKeepMtz = findPref(R.string.id_keep_mtz);
        pRoot25 = findPref(R.string.id_root25);
        pNoUpdate = findPref(R.string.id_noupdate);

        // about
        pFeedback = findPref(R.string.id_feedback);
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
        pMemory.setOnPreferenceClickListener(this);

        // miui
        pTheme.setOnPreferenceClickListener(this);
        pRemoveAd.setOnPreferenceClickListener(this);
        pRemoveSearch.setOnPreferenceClickListener(this);
        pMinusScreen.setOnPreferenceClickListener(this);
        pKeepMtz.setOnPreferenceClickListener(this);
        pRoot25.setOnPreferenceClickListener(this);
        pNoUpdate.setOnPreferenceClickListener(this);

        // about
        pFeedback.setOnPreferenceClickListener(this);
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
        miSettings = menu.add(0, 2, 0, R.string.ab_settings);
        miSettings.setIcon(android.R.drawable.ic_menu_preferences);
        miSettings.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        miShare = menu.add(0, 1, 1, R.string.ab_help);
        miShare.setIcon(android.R.drawable.ic_menu_help);
        miShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case 1:
                showQQGroup();
                break;
            case 2:
                showActivityResult(SettingsActivity.class, 1);
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

        pRemoveSearch.setStatus(pref.getBoolean(XpStatus.KEY_REMOVESEARCHBAR, false));
        pMinusScreen.setStatus(pref.getBoolean(XpStatus.KEY_MINUS_SCREEN, false));
        pKeepMtz.setStatus(pref.getBoolean(XpStatus.KEY_KEEP_MTZ, false));
        pRoot25.setStatus(pref.getBoolean(XpStatus.KEY_ROOTCRACK, false));
        pCoreCrack.setStatus(pref.getBoolean(XpStatus.KEY_CORECRACK, false));
        pNoUpdate.setStatus(pref.getBoolean(XpStatus.KEY_NOUPDATE, false));

        if (pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
            pRemoveAd.setShowSwitch(false);
        } else {
            pRemoveAd.setShowSwitch(true);
            pRemoveAd.setStatus(pref.getBoolean(XpStatus.KEY_REMOVEAD, false));
        }

    }

    private void setXposedRootStatus() {

        boolean isMIUI = AppUtils.isMIUI(getContext());

        pTerminal.setEnabled(true);
        pFeedback.setEnabled(true);
        pAbout.setEnabled(true);

        pFreeze.setEnabled(!DeviceAPI.isRejected());
        pComponent.setEnabled(!DeviceAPI.isRejected());
        pMemory.setEnabled(!DeviceAPI.isRejected());
        pCleanArt.setEnabled(!DeviceAPI.isRejected());
        pFakeDevice.setEnabled(!DeviceAPI.isRejected());
        pNoUpdate.setEnabled(isMIUI && !DeviceAPI.isRejected());

        if (Build.VERSION.SDK_INT >= 24) {
            pTheme.setEnabled(isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected());
            pRemoveSearch.setEnabled(isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected());
            pMinusScreen.setEnabled(isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected());
            pKeepMtz.setEnabled(isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected());
            pRoot25.setEnabled(isMIUI && XpStatus.isEnable() && !DeviceAPI.isRejected());
            pCoreCrack.setEnabled(XpStatus.isEnable() && !DeviceAPI.isRejected());
        } else {
            pTheme.setEnabled(isMIUI && XpStatus.isEnable());
            pRemoveSearch.setEnabled(isMIUI && XpStatus.isEnable());
            pMinusScreen.setEnabled(isMIUI && XpStatus.isEnable());
            pKeepMtz.setEnabled(isMIUI && XpStatus.isEnable());
            pRoot25.setEnabled(isMIUI && XpStatus.isEnable());
            pCoreCrack.setEnabled(XpStatus.isEnable());
        }
        pRemoveAd.setEnabled(isMIUI && XpStatus.isEnable());
        if (!isMIUI) {
            getPreferenceScreen().removePreference(catMiui);
            catAbout.removePreference(pFeedback);
        }
    }

    private void showActivity(Class<?> cls) {
        Intent inA = new Intent(getContext(), cls);
        startActivity(inA);
    }

    private void showActivityResult(Class<?> cls, int req) {
        Intent inA = new Intent(getContext(), cls);
        startActivityForResult(inA, req);
    }

    private void threadWriteHost() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                HostsUtils.writeHost(getContext(), pref.getBoolean(XpStatus.KEY_NOUPDATE, false), pref.getBoolean(XpStatus.KEY_REMOVEAD, false));
            }
        }).start();
    }

    private void threadDeleteTmpFiles() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                DeviceAPI.forceDeleteFile("/data/data/com.miui.cleanmaster/shared_prefs/*");
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
        } else if (prefKey.equals(getString(R.string.id_feedback))) {
            showActivity(FeedbackActivity.class);
        } else if (prefKey.equals(getString(R.string.id_fake_device))) {
            showActivity(FakeDeviceActivity.class);
        } else if (prefKey.equals(getString(R.string.id_memory))) {
            threadMemory();
        } else if (prefKey.equals(getString(R.string.id_theme))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_THEMECRACK, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_removead))) {
            if (pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
                showActivity(MIUIAppSettingActivity.class);
            } else {
                ex.setStatus(!ex.getStatus());
                editor.putBoolean(XpStatus.KEY_REMOVEAD, ex.getStatus()).apply();
                DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
                threadWriteHost();
                threadDeleteTmpFiles();
            }
        } else if (prefKey.equals(getString(R.string.id_removesearch))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_REMOVESEARCHBAR, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_minus_screen))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_MINUS_SCREEN, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_keep_mtz))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_KEEP_MTZ, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_root25))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_ROOTCRACK, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_corecrack))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_CORECRACK, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
        } else if (prefKey.equals(getString(R.string.id_noupdate))) {
            ex.setStatus(!ex.getStatus());
            editor.putBoolean(XpStatus.KEY_NOUPDATE, ex.getStatus()).apply();
            DeviceAPI.makePreferenceReadable(Build.VERSION.SDK_INT, getContext().getPackageName());
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

    private void showQQGroup() {
        // show qq group
        ImageView ivLogo = new ImageView(getContext());
        ivLogo.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
        int w = (int) (UIUtils.getWidth() * 0.8);
        ivLogo.setLayoutParams(new ViewGroup.LayoutParams(w, w));
        ivLogo.setImageResource(R.drawable.qqgroup);
        new AlertDialog.Builder(getContext())
                .setTitle(R.string.alert_welcome)
                .setView(ivLogo)
                .setMessage(R.string.alert_qq_group)
                .setPositiveButton(R.string.alert_ok, null)
                .show();
    }

    private Handler hMemory = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            pMemory.setEnabled(true);
            Toast.makeText(getContext(), R.string.toast_memory_cleaned, Toast.LENGTH_SHORT).show();
            super.handleMessage(msg);
        }
    };

    private void threadMemory() {
        pMemory.setEnabled(false);
        new Thread(new Runnable() {
            @Override
            public void run() {
                DeviceAPI.killProcess();
                if (pref.getBoolean(XpStatus.KEY_DEEP_CLEAN, false)) {
                    DeviceAPI.forceDropCache();
                }
                hMemory.sendEmptyMessage(0);
            }
        }).start();
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == 1) {
            if (pref.getBoolean(XpStatus.KEY_AD_CHOOSE, false)) {
                pRemoveAd.setShowSwitch(false);
            } else {
                pRemoveAd.setShowSwitch(true);
                pRemoveAd.setStatus(pref.getBoolean(XpStatus.KEY_REMOVEAD, false));
            }
        }
    }
}
