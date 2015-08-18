package com.yugioh.android.fragments;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.ZipUtils;
import com.yugioh.android.R;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.common.Actions;
import com.yugioh.android.database.YugiohDatabase;
import com.yugioh.android.database.YugiohUtils;
import com.yugioh.android.define.NetworkDefine;
import com.yugioh.android.define.PathDefine;
import com.yugioh.android.intf.IDestroyCallback;
import com.yugioh.android.intf.IUpdateIntf;
import com.yugioh.android.utils.UpdateUtils;
import com.yugioh.android.utils.YGOAPI;

import java.io.File;

public class UpdateFragment extends BaseFragment implements IDestroyCallback, OnClickListener {

    final String dbSource = PathDefine.DOWNLOAD_PATH + PathDefine.DATA_ZIP;
    final String apkSource = PathDefine.DOWNLOAD_PATH + PathDefine.APK_NAME;
    Button btnUpdateApk, btnUpdateData;
    TextView tvApkInfo, tvDataInfo;
    ProgressBar pbDownlaodingApk, pbDownlaodingData;
    RelativeLayout layUpdateApk, layNoDatabase;
    TextView tvUpdateLogValue;
    UpdateInfo updateInfo = null;
    boolean hasData = YugiohDatabase.isDatabaseFileExists();
    private Handler hApkTask = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (getActivity() != null) {
                switch (msg.what) {
                    case DownloadUtils.WHAT_DOWNLOAD_START:
                    case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
                        pbDownlaodingApk.setMax(msg.arg2);
                        pbDownlaodingApk.setProgress(msg.arg1);
                        break;
                    case DownloadUtils.WHAT_DOWNLOAD_FINISH:
                        try {
                            pbDownlaodingApk.setVisibility(View.GONE);
                            ((IUpdateIntf) getActivity()).setInProgress(false);
                            updateInfo.setUpdateApk(-1);
                            updateCurrentStatus();
                            updateDisabled(true);

                        } catch (Exception e) {

                        }
                        break;
                }
            }
            super.handleMessage(msg);
        }
    };
    private Handler hDataTask = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            switch (msg.what) {
                case DownloadUtils.WHAT_DOWNLOAD_START:
                case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
                    pbDownlaodingData.setMax(msg.arg2);
                    pbDownlaodingData.setProgress(msg.arg1);
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_FINISH:
                    unzipDataT();
                    break;
            }
            super.handleMessage(msg);
        }
    };
    private Handler hUnzip = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                pbDownlaodingData.setVisibility(View.GONE);
                ((IUpdateIntf) getActivity()).setInProgress(false);
                updateInfo.setUpdateData(0);
                updateCurrentStatus();
                updateDisabled(true);
                confirmClose();
            }
            super.handleMessage(msg);
        }
    };

    final Handler hUpdate = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                updateInfo = (UpdateInfo) msg.obj;
                showUpdateInfo(updateInfo);
            }
            super.handleMessage(msg);
        }
    };

    public UpdateFragment() {
        super();
    }

    @Override
    public int getBarTitle() {
        return R.string.page_update;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.page_update;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_update;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initComponents() {
        layUpdateApk = (RelativeLayout) innerView.findViewById(R.id.layUpdateApk);
        layNoDatabase = (RelativeLayout) innerView.findViewById(R.id.layNoDatabase);
        btnUpdateApk = (Button) innerView.findViewById(R.id.btnUpdateApk);
        btnUpdateData = (Button) innerView.findViewById(R.id.btnUpdateData);
        tvApkInfo = (TextView) innerView.findViewById(R.id.tvApkInfo);
        tvDataInfo = (TextView) innerView.findViewById(R.id.tvDataInfo);
        pbDownlaodingApk = (ProgressBar) innerView.findViewById(R.id.pbDownlaodingApk);
        pbDownlaodingData = (ProgressBar) innerView.findViewById(R.id.pbDownlaodingData);
        tvUpdateLogValue = (TextView) innerView.findViewById(R.id.tvUpdateLogValue);
    }

    @Override
    public void initEvents() {
        btnUpdateApk.setOnClickListener(this);
        btnUpdateData.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        File fDownload = new File(PathDefine.DOWNLOAD_PATH);
        if (!fDownload.exists()) {
            fDownload.mkdirs();
        }
        getUpdateLogT();
        UpdateUtils.checkUpdateT(getActivity(), hUpdate);
    }

    private void getUpdateLogT() {
        final Handler hLog = new Handler() {
            @Override
            public void handleMessage(Message msg) {
                if (msg.what == 1) {
                    UpdateFragment.this.tvUpdateLogValue.setText((String) msg.obj);
                }
                super.handleMessage(msg);
            }
        };
        new Thread(new Runnable() {
            @Override
            public void run() {
                String ret = YGOAPI.getUpdateLog();
                Message msg = new Message();
                msg.what = 1;
                msg.obj = ret;
                hLog.sendMessage(msg);
            }
        }).start();
    }

    private void showUpdateInfo(UpdateInfo info) {
        updateInfo = info;
        updateCurrentStatus();
        updateDisabled(true);
    }

    private void updateCurrentStatus() {

        tvApkInfo.setVisibility(View.VISIBLE);
        tvDataInfo.setVisibility(View.VISIBLE);
        if (hasData) {
            switch (updateInfo.getUpdateApk()) {
                case -1:
                    tvApkInfo.setText(getString(R.string.update_apk_fmt, updateInfo.getApkVersion()));
                    btnUpdateApk.setText(R.string.update_install);
                    break;
                case 0:
                    tvApkInfo.setText(R.string.update_no_apk);
                    btnUpdateApk.setText(R.string.update_renew);
                    break;
                default:
                    tvApkInfo.setText(getString(R.string.update_apk_fmt, updateInfo.getApkVersion()));
                    btnUpdateApk.setText(R.string.update_renew);
                    break;

            }
        }
        switch (updateInfo.getUpdateData()) {
            case 0:
                tvDataInfo.setText(R.string.update_no_data);
                break;
            default:
                if (!hasData) {
                    tvDataInfo.setText(R.string.update_data_full);
                } else {
                    tvDataInfo.setText(getString(R.string.update_data_fmt, updateInfo.getNewCard()));
                }
                break;
        }

        if (!hasData) {
            layUpdateApk.setVisibility(View.GONE);
            layNoDatabase.setVisibility(View.VISIBLE);
        } else {
            layUpdateApk.setVisibility(View.VISIBLE);
            layNoDatabase.setVisibility(View.GONE);
        }

    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    private void unzipDataT() {

        new Thread(new Runnable() {

            @Override
            public void run() {
                try {
                    YugiohUtils.closeDatabase(getActivity());
                    FileUtils.deleteFile(PathDefine.DATABASE_PATH);
                    ZipUtils.unzipFile(new File(dbSource), PathDefine.ROOT_PATH);
                    FileUtils.deleteFile(dbSource);
                    YugiohUtils.newDatabase(getActivity());
                    hUnzip.sendEmptyMessage(1);
                } catch (Exception e) {

                }

            }
        }).start();

    }

    private void confirmClose() {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.update_download_data_finish)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        YugiohUtils.closeDatabase(getActivity());
                        YugiohUtils.newDatabase(getActivity());
                        getActivity().finish();
                        getActivity().sendBroadcast(new Intent(Actions.ACTION_CLOSE_MAIN));
                    }
                })
                .show();
    }

    private void installApk() {
        File fApk = new File(apkSource);
        if (fApk.exists()) {
            Uri uri = Uri.fromFile(fApk);
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setDataAndType(uri, "application/vnd.android.package-archive");
            startActivity(intent);
        }
    }

    @Override
    public void onClick(View v) {
        ((IUpdateIntf) getActivity()).setInProgress(true);
        updateDisabled(false);
        switch (v.getId()) {
            case R.id.btnUpdateApk:
                if (updateInfo.getUpdateApk() == -1) {
                    installApk();
                } else {
                    ((IUpdateIntf) getActivity()).setUpdateFile(PathDefine.DOWNLOAD_PATH, PathDefine.APK_NAME);
                    tvApkInfo.setVisibility(View.GONE);
                    FileUtils.deleteFile(apkSource);
                    pbDownlaodingApk.setVisibility(View.VISIBLE);
                    DownloadUtils.downloadFileT(getActivity(), null, NetworkDefine.URL_APK, PathDefine.DOWNLOAD_PATH, PathDefine.APK_NAME, hApkTask);
                }
                break;
            case R.id.btnUpdateData:
                ((IUpdateIntf) getActivity()).setUpdateFile(PathDefine.DOWNLOAD_PATH, PathDefine.DATA_ZIP);
                tvDataInfo.setVisibility(View.GONE);
                FileUtils.deleteFile(dbSource);
                pbDownlaodingData.setVisibility(View.VISIBLE);
                DownloadUtils.downloadFileT(getActivity(), null, NetworkDefine.URL_DATA, PathDefine.DOWNLOAD_PATH, PathDefine.DATA_ZIP, hDataTask);
                break;
        }
    }

    private void updateDisabled(boolean enabled) {
        btnUpdateApk.setEnabled(false);
        btnUpdateData.setEnabled(false);
        if (enabled) {

            if (updateInfo.getUpdateApk() != 0) {
                btnUpdateApk.setEnabled(true);
            }
            if (updateInfo.getUpdateData() != 0) {
                btnUpdateData.setEnabled(true);
            }
        }
    }

    @Override
    public void doDestroyHandler() {
        hApkTask = null;
        hDataTask = null;

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
