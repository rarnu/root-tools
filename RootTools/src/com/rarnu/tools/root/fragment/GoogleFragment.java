package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Loader;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.GoogleAdapter;
import com.rarnu.tools.root.adapter.GoogleSdkAdapter;
import com.rarnu.tools.root.api.MobileGoogleApi;
import com.rarnu.tools.root.common.GoogleInfo;
import com.rarnu.tools.root.common.GooglePackageInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.GoogleLoader;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.GoogleUtils;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.FileUtils;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class GoogleFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<GoogleInfo>>, AdapterView.OnItemSelectedListener, View.OnClickListener {

    TextView tvSdkVer;
    ListView lvGoogle;
    RelativeLayout layDownload;
    ProgressBar pbDownloading;
    TextView tvDownloading;
    TextView tvPercent;
    DataProgressBar progressGoogle;
    List<GoogleInfo> list;
    GoogleAdapter adapter;
    GoogleLoader loader;
    Spinner spVersion;
    GoogleSdkAdapter adapterSdk;
    List<GooglePackageInfo> listSdk;
    Button btnInstall;
    int[] sdkVers = new int[]{15, 16, 17, 18, 19};
    boolean supportted = false;
    MenuItem miDownload;
    boolean downloading = false;
    Thread thDownload = null;
    private Handler hDownload = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            switch (msg.what) {
                case DownloadUtils.WHAT_DOWNLOAD_START:
                    pbDownloading.setMax(msg.arg2);
                    pbDownloading.setProgress(msg.arg1);
                    tvPercent.setText("0%");
                    tvDownloading.setText(String.format("%d / %d", msg.arg1, msg.arg2));
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
                    pbDownloading.setProgress(msg.arg1);
                    tvPercent.setText(String.format("%d%%", (int) (msg.arg1 * 100.0D / msg.arg2)));
                    tvDownloading.setText(String.format("%d / %d", msg.arg1, msg.arg2));
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_FINISH:
                    pbDownloading.setProgress(pbDownloading.getMax());
                    tvPercent.setText("100%");
                    tvDownloading.setText(R.string.unzipping);
                    doUnzipT();
                    break;
            }
            super.handleMessage(msg);
        }
    };
    private Handler hUnzip = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                downloading = false;
                layDownload.setVisibility(View.GONE);
                lvGoogle.setEnabled(true);
                spVersion.setEnabled(true);
                btnInstall.setEnabled(true);
                if (miDownload != null) {
                    miDownload.setEnabled(true);
                }
                doStartLoading(((GooglePackageInfo) spVersion.getSelectedItem()).sdk_version);

            }
            super.handleMessage(msg);
        }
    };
    private Handler hInstallGoogle = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                progressGoogle.setAppName(getString(R.string.google_download_check));
                spVersion.setEnabled(true);
                if (miDownload != null) {
                    miDownload.setEnabled(true);
                }
                btnInstall.setEnabled(true);
                doStartLoading(((GooglePackageInfo) spVersion.getSelectedItem()).sdk_version);
                new AlertDialog.Builder(getActivity())
                        .setTitle(R.string.hint)
                        .setMessage(R.string.google_reboot_hint)
                        .setPositiveButton(R.string.ok, null)
                        .show();
            }
            super.handleMessage(msg);
        }
    };

    @Override
    public int getBarTitle() {
        return R.string.func_google;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_google_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        tvSdkVer = (TextView) innerView.findViewById(R.id.tvSdkVer);
        lvGoogle = (ListView) innerView.findViewById(R.id.lvGoogle);
        layDownload = (RelativeLayout) innerView.findViewById(R.id.layDownload);
        pbDownloading = (ProgressBar) innerView.findViewById(R.id.pbDownloading);
        tvDownloading = (TextView) innerView.findViewById(R.id.tvDownloading);
        tvPercent = (TextView) innerView.findViewById(R.id.tvPercent);
        progressGoogle = (DataProgressBar) innerView.findViewById(R.id.progressGoogle);
        spVersion = (Spinner) innerView.findViewById(R.id.spVersion);
        btnInstall = (Button) innerView.findViewById(R.id.btnInstall);
        list = new ArrayList<GoogleInfo>();
        adapter = new GoogleAdapter(getActivity(), list);
        lvGoogle.setAdapter(adapter);
        loader = new GoogleLoader(getActivity());

        progressGoogle.setAppName(getString(R.string.google_download_check));

        listSdk = new ArrayList<GooglePackageInfo>();
        adapterSdk = new GoogleSdkAdapter(getActivity(), listSdk);
        spVersion.setAdapter(adapterSdk);

    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
        spVersion.setOnItemSelectedListener(this);
        btnInstall.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        doLoadSdks();
        showSdkVersion();
        doStartLoading(Build.VERSION.SDK_INT);
    }

    private void doLoadSdks() {
        int idx = 0;
        for (int i = 0; i < sdkVers.length; i++) {
            if (Build.VERSION.SDK_INT == sdkVers[i]) {
                idx = i;
            }
            try {
                listSdk.add(GooglePackageInfo.fromJson(FileUtils.readAssetFile(getActivity(), String.format("google_%d", sdkVers[i]))));
            } catch (IOException e) {
            }
        }
        adapterSdk.setNewList(listSdk);
        spVersion.setSelection(idx);
        // spVersion.setEnabled(idx != -1);
    }

    private void doStartLoading(int sdkint) {
        if (getActivity() != null) {
            try {
                String jsonString = FileUtils.readAssetFile(getActivity(), String.format("google_%d", sdkint));
                GooglePackageInfo packageItem = GooglePackageInfo.fromJson(jsonString);
                loader.setData(packageItem, sdkint);
                loader.startLoading();
                supportted = true;
            } catch (Exception e) {
                supportted = false;
            }
        }
    }

    private void showSdkVersion() {
        int matchRes = (Build.VERSION.SDK_INT == ((GooglePackageInfo) spVersion.getSelectedItem()).sdk_version) ? R.string.google_sdk_match : R.string.google_sdk_unmatch;
        String sdk = getString(R.string.google_current_sdk, Build.VERSION.RELEASE, Build.VERSION.SDK_INT, getString(matchRes));
        tvSdkVer.setText(sdk);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_google;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miDownload = menu.add(0, MenuItemIds.MENU_DOWNLOAD, 99, R.string.download);
        miDownload.setIcon(R.drawable.ic_menu_attachment);
        miDownload.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_DOWNLOAD:
                if (!supportted) {
                    new AlertDialog.Builder(getActivity())
                            .setTitle(R.string.hint)
                            .setMessage(R.string.google_not_supportted)
                            .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                                @Override
                                public void onClick(DialogInterface dialog, int which) {
                                    doDownloadT();
                                }
                            })
                            .setNegativeButton(R.string.cancel, null)
                            .show();
                } else {
                    doDownloadT();
                }
                break;
        }
        return true;
    }

    private void doUnzipT() {
        final int sdkVer = ((GooglePackageInfo) spVersion.getSelectedItem()).sdk_version;
        final String fileName = DirHelper.GOOGLE_DIR + String.format("google_%d.zip", sdkVer);
        new Thread(new Runnable() {
            @Override
            public void run() {
                // unzip
                Message msg = new Message();
                msg.what = 1;
                try {
                    File fExtractDir = new File(DirHelper.GOOGLE_DIR + sdkVer + "/");
                    if (!fExtractDir.exists()) {
                        fExtractDir.mkdirs();
                    }
                    String cmd = String.format("busybox unzip -o \"%s\" -d \"%s\"", fileName, DirHelper.GOOGLE_DIR + sdkVer + "/");
                    CommandResult result = RootUtils.runCommand(cmd, true);

                    if (result != null && result.error.equals("")) {
                        msg.arg1 = 1;
                    } else {
                        msg.arg1 = 0;
                    }
                } catch (Exception e) {

                }
                hUnzip.sendMessage(msg);
            }
        }).start();
    }

    private void doDownloadT() {
        final int selectSdk = ((GooglePackageInfo) spVersion.getSelectedItem()).sdk_version;
        final String fileName = DirHelper.GOOGLE_DIR + String.format("google_%d.zip", selectSdk);

        final String url = MobileGoogleApi.getDownloadUrl(selectSdk);
        downloading = true;
        pbDownloading.setProgress(0);
        tvDownloading.setText("");
        layDownload.setVisibility(View.VISIBLE);
        lvGoogle.setEnabled(false);
        spVersion.setEnabled(false);
        btnInstall.setEnabled(false);
        if (miDownload != null) {
            miDownload.setEnabled(false);
        }

        if (new File(fileName).exists()) {
            pbDownloading.setProgress(pbDownloading.getMax());
            tvDownloading.setText(R.string.unzipping);
            tvPercent.setText("100%");
            doUnzipT();
            return;
        }

        thDownload = new Thread(new Runnable() {
            @Override
            public void run() {
                DownloadUtils.downloadFile(url, DirHelper.GOOGLE_DIR + String.format("google_%d.zip", selectSdk), hDownload);
            }
        });
        thDownload.start();

    }

    @Override
    public void onGetNewArguments(Bundle bn) {
        if (bn != null && bn.getBoolean("cancel", true)) {
            try {
                thDownload.wait(5000);
                thDownload.interrupt();
            } catch (Exception e) {

            }
        }
    }

    @Override
    public Bundle getFragmentState() {
        Bundle bn = new Bundle();
        bn.putBoolean("downloading", downloading);
        return bn;
    }

    @Override
    public void onLoadComplete(Loader<List<GoogleInfo>> loader, List<GoogleInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
        }
    }

    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
        showSdkVersion();
        doStartLoading(((GooglePackageInfo) spVersion.getSelectedItem()).sdk_version);
    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {

    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnInstall:
                if (!checkZipExist()) {
                    return;
                }
                int mode = checkFileCorrect();
                if (mode == -1) {
                    return;
                }
                confirmInstallGoogle(mode);
                break;
        }
    }

    private boolean checkZipExist() {
        int selectSdk = ((GooglePackageInfo) spVersion.getSelectedItem()).sdk_version;
        String fileName = DirHelper.GOOGLE_DIR + String.format("google_%d.zip", selectSdk);
        boolean ret = true;
        if (!new File(fileName).exists()) {
            new AlertDialog.Builder(getActivity())
                    .setTitle(R.string.hint)
                    .setMessage(R.string.google_no_zip)
                    .setPositiveButton(R.string.ok, null)
                    .show();
            ret = false;
        }
        return ret;
    }

    private int checkFileCorrect() {
        int mode = 0;
        if (GoogleUtils.isAllFilesCorrect(list)) {
            mode = 1;
            if (GoogleUtils.isAllOptionalFilesCorrect(list)) {
                new AlertDialog.Builder(getActivity())
                        .setTitle(R.string.hint)
                        .setMessage(R.string.google_all_files_correct)
                        .setPositiveButton(R.string.ok, null)
                        .show();
                mode = -1;
            }
        }
        return mode;
    }

    /**
     * @param mode 0:all, 1:optional only
     */
    private void confirmInstallGoogle(final int mode) {
        final View vInstallDialog = LayoutInflater.from(getActivity()).inflate(R.layout.dialog_install_google, null);
        if (mode == 1) {
            vInstallDialog.findViewById(R.id.chkOverrideBrokenFile).setVisibility(View.GONE);
        }
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.google_install_dialog)
                .setView(vInstallDialog)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        boolean obf = ((CheckBox) vInstallDialog.findViewById(R.id.chkOverrideBrokenFile)).isChecked();
                        boolean io = ((CheckBox) vInstallDialog.findViewById(R.id.chkInstallOptional)).isChecked();
                        doInstallGoogleT(obf, io, mode);
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void doInstallGoogleT(final boolean overrideBroken, final boolean installOptional, int mode) {

        final List<GoogleInfo> installList = GoogleUtils.getInstallFileList(list, overrideBroken, installOptional, mode);
        if (installList == null || installList.size() == 0) {
            return;
        }

        progressGoogle.setAppName(getString(R.string.installing_system_app));
        spVersion.setEnabled(false);
        if (miDownload != null) {
            miDownload.setEnabled(false);
        }
        btnInstall.setEnabled(false);

        new Thread(new Runnable() {
            @Override
            public void run() {
                int selectSdk = ((GooglePackageInfo) spVersion.getSelectedItem()).sdk_version;
                String cmd = "";
                String fileName = "";
                String xmlFolder = "";
                for (GoogleInfo gi : installList) {
                    // install google package
                    switch (gi.type) {
                        case 0:
                            fileName = "/system/app/" + gi.fileName;
                            break;
                        case 1:
                            fileName = "/system/framework/" + gi.fileName;
                            break;
                        case 2:
                            fileName = "/system/lib/" + gi.fileName;
                            break;
                        case 3:
                            xmlFolder = "/system/etc/" + gi.path + "/";
                            if (!new File(xmlFolder).exists()) {
                                RootUtils.runCommand("mkdir " + xmlFolder, true);
                                RootUtils.runCommand("chmod 755 " + xmlFolder, true);
                            }
                            fileName = xmlFolder + gi.fileName;
                            break;
                    }
                    cmd = "busybox cp ";
                    if (gi.type == 3) {
                        cmd += DirHelper.GOOGLE_DIR + selectSdk + "/" + gi.path + "/" + gi.fileName;
                    } else {
                        cmd += DirHelper.GOOGLE_DIR + selectSdk + "/" + gi.fileName;
                    }
                    cmd += " " + fileName;
                    CommandResult result = RootUtils.runCommand(cmd, true);
                    if (result.error.equals("")) {
                        RootUtils.runCommand("chmod 644 " + fileName, true);
                    }
                }
                hInstallGoogle.sendEmptyMessage(1);
            }
        }).start();
    }
}
