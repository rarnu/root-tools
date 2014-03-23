package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Html;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.service.ScreenService;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.os.BreakableThread;

import java.io.File;

public class UpdateFragment extends BaseFragment implements View.OnClickListener, BreakableThread.RunningCallback {

    TextView tvUpdateTitle;
    TextView tvUpdateDesc;
    ProgressBar pbDownload;
    TextView tvDownload;
    Button btnDownload;
    TextView tvCannotUpdate;
    boolean isDownloading = false;
    private Handler hProgress = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (getActivity() != null) {
                switch (msg.what) {
                    case DownloadUtils.WHAT_DOWNLOAD_START:

                        pbDownload.setMax(msg.arg2);
                        pbDownload.setProgress(0);
                        tvDownload.setText(getString(R.string.toast_downloading, 0));
                        pbDownload.setVisibility(View.VISIBLE);
                        tvDownload.setVisibility(View.VISIBLE);
                        break;
                    case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
                        pbDownload.setProgress(msg.arg1);
                        tvDownload.setText(getString(R.string.toast_downloading, (int) (msg.arg1 * 1D / msg.arg2 * 100)));
                        break;
                    case DownloadUtils.WHAT_DOWNLOAD_FINISH:
                        isDownloading = false;
                        pbDownload.setProgress(pbDownload.getMax());
                        tvDownload.setText("");
                        installUpdateT();
                        break;
                }
            }
            super.handleMessage(msg);
        }
    };
    BreakableThread thDownload = null;

    @Override
    public int getBarTitle() {
        return R.string.check_update;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.check_update;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        tvUpdateTitle = (TextView) innerView.findViewById(R.id.tvUpdateTitle);
        tvUpdateDesc = (TextView) innerView.findViewById(R.id.tvUpdateDesc);
        pbDownload = (ProgressBar) innerView.findViewById(R.id.pbDownload);
        tvDownload = (TextView) innerView.findViewById(R.id.tvDownload);
        btnDownload = (Button) innerView.findViewById(R.id.btnDownload);
        tvCannotUpdate = (TextView) innerView.findViewById(R.id.tvCannotUpdate);
    }

    @Override
    public void initEvents() {
        btnDownload.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        tvUpdateTitle.setText(getString(R.string.update_title, GlobalInstance.updateInfo.versionName, GlobalInstance.updateInfo.size));
        tvUpdateDesc.setText(Html.fromHtml(GlobalInstance.updateInfo.desc));

        tvCannotUpdate.setVisibility(GlobalInstance.isOfficialVersion ? View.GONE : View.VISIBLE);
        btnDownload.setEnabled(GlobalInstance.isOfficialVersion);

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_update;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {
        if (bn != null && bn.getBoolean("cancel", true)) {
            try {
                if (thDownload != null) {
                    isDownloading = false;
                }
            } catch (Exception e) {

            }
        }
    }

    @Override
    public Bundle getFragmentState() {
        Bundle bn = new Bundle();
        bn.putBoolean("downloading", isDownloading);
        return bn;
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnDownload:

                doDownloadT();
                break;
        }
    }

    private void doDownloadT() {
        // download update
        isDownloading = true;
        btnDownload.setEnabled(false);
        thDownload = new BreakableThread(this) {
            @Override
            public void run() {
                String downUrl = MobileApi.DOWNLOAD_BASE_URL + GlobalInstance.updateInfo.file;
                String localFile = DirHelper.TEMP_DIR + GlobalInstance.updateInfo.file;
                File fApk = new File(localFile);
                if (fApk.exists()) {
                    fApk.delete();
                }
                DownloadUtils.downloadFile(downUrl, localFile, hProgress, getRunningCallback());
            }
        };
        thDownload.start();

    }

    private void installUpdateT() {
        final String localFile = DirHelper.TEMP_DIR + GlobalInstance.updateInfo.file;
        if (!new File(localFile).exists()) {
            Toast.makeText(getActivity(), R.string.toast_download_update_failed, Toast.LENGTH_LONG).show();
            return;
        }
        btnDownload.setEnabled(true);
        getActivity().stopService(new Intent(getActivity().getBaseContext(), ScreenService.class));
        ApkUtils.openInstallApk(getActivity(), localFile);

    }

    @Override
    public boolean getRunningState() {
        return isDownloading;
    }
}
