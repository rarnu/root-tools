package com.rarnu.tools.root.fragment;

import android.content.pm.ApplicationInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Html;
import android.text.Spanned;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.utils.ApkUtils;

public class InstallApkFragment extends BaseDialogFragment implements View.OnClickListener {

    TextView tvInstallingApk;
    Button btnOK;
    ProgressBar pbInstalling;
    ApplicationInfo appInfo = null;
    String filePath = "";
    int mode = 0;
    private Handler hInstalled = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                String ret = (String) msg.obj;
                showInstallResult(ret);
            }
            super.handleMessage(msg);
        }
    };

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        tvInstallingApk = (TextView) innerView.findViewById(R.id.tvInstallingApk);
        btnOK = (Button) innerView.findViewById(R.id.btnOK);
        pbInstalling = (ProgressBar) innerView.findViewById(R.id.pbInstalling);
    }

    @Override
    public void initEvents() {
        btnOK.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        filePath = getActivity().getIntent().getStringExtra("filePath");
        // mode 0:normal install 1:force install
        mode = getActivity().getIntent().getIntExtra("mode", 0);

        appInfo = ApkUtils.getAppInfoFromPackage(filePath);
        if (appInfo != null) {
            tvInstallingApk.setText(getString(R.string.installing_apk_fmt, appInfo.packageName));
            doInstallT(filePath, mode);
        } else {
            getActivity().finish();
        }
    }

    private void showInstallResult(String ret) {
        ret = ret.replace("pkg:", "").replace(filePath, "").trim();
        pbInstalling.setVisibility(View.INVISIBLE);
        btnOK.setVisibility(View.VISIBLE);
        Spanned retStr = null;
        if (ret.equals("")) {
            retStr = Html.fromHtml(getString(R.string.install_apk_finished, appInfo.packageName));
        } else {
            ret = ret.replace("Failure", "").replace("[", "").replace("]", "").replace("_", " ").toLowerCase().trim();
            String html = getString(R.string.install_apk_failed, ret);
            retStr = Html.fromHtml(html);
        }
        tvInstallingApk.setText(retStr);

    }

    private void doInstallT(final String filePath, final int mode) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                String ret = "";
                switch (mode) {
                    case 0:
                        ret = ApkUtils.installAppWithResult(filePath);
                        break;
                    case 1:
                        ret = ApkUtils.forceInstallAppWithResult(filePath);
                        break;
                }
                Message msg = new Message();
                msg.what = 1;
                msg.obj = ret;
                hInstalled.sendMessage(msg);
            }
        }).start();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_install_apk;
    }

    @Override
    public String getMainActivityName() {
        return "";
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
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnOK:
                getActivity().finish();
                break;
        }
    }
}
