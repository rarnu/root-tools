package com.rarnu.tools.neo.fragment;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.*;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ScrollView;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.api.NativeAPI;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.utils.FileUtils;

import java.io.File;

public class CleanFragment extends BaseFragment {

    public static final String ACTION_CLEAN_CALLBACK = "com.rarnu.tools.neo.clean.callback";
    public static final String KEY_STATUS = "status";
    public static final String KEY_DATA = "data";

    private TextView tvClean = null;
    private MenuItem miRun = null;
    private ScrollView svClean = null;
    private boolean isCleaning = false;

    private IntentFilter filterCallback = null;
    private CleanCallbackReceiver receiverCallback = null;

    @Override
    public int getBarTitle() {
        return R.string.clean_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        svClean = (ScrollView) innerView.findViewById(R.id.svClean);
        tvClean = (TextView) innerView.findViewById(R.id.tvClean);
    }

    @Override
    public void initEvents() {

    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        filterCallback = new IntentFilter(ACTION_CLEAN_CALLBACK);
        receiverCallback = new CleanCallbackReceiver();
        getActivity().registerReceiver(receiverCallback, filterCallback);
    }

    @Override
    public void onDestroy() {
        getActivity().unregisterReceiver(receiverCallback);
        super.onDestroy();
    }

    @Override
    public void initLogic() {
        boolean busyboxExists = new File("/system/bin/busybox").exists() || new File("/system/xbin/busybox").exists();
        boolean duExists = new File("/system/bin/du").exists() || new File("/system/xbin/du").exists();
        if (duExists) {
            tvClean.setText(R.string.view_ready);
            return;
        }
        if (busyboxExists) {
            tvClean.setText(R.string.view_ready);
            return;
        }
        tvClean.setText(R.string.view_not_ready);
        threadExtractBusybox();
    }

    private Handler hEnvReady = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 0) {
                tvClean.setText(R.string.view_env_error);
                if (miRun != null) {
                    miRun.setEnabled(false);
                }
            } else {
                tvClean.setText(R.string.view_ready);
            }
            super.handleMessage(msg);
        }
    };

    private void threadExtractBusybox() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                // extract busybox
                String[] abis = Build.SUPPORTED_ABIS;
                String busyboxAsset = "busybox_arm";
                for (String abi: abis) {
                    if (abi.toLowerCase().contains("mips")) {
                        busyboxAsset = "busybox_mips";
                        break;
                    }
                    if (abi.toLowerCase().contains("x86")) {
                        busyboxAsset = "busybox_x86";
                        break;
                    }
                }
                String tmpDir = Environment.getExternalStorageDirectory().getAbsolutePath();
                File fDir = new File(tmpDir, ".tmp");
                if (!fDir.exists()) {
                    fDir.mkdirs();
                }
                File fBusybox = new File(fDir, busyboxAsset);
                FileUtils.copyAssetFile(getContext(), busyboxAsset, fDir.getAbsolutePath());
                NativeAPI.mount();
                boolean ret = NativeAPI.catFile(fBusybox.getAbsolutePath(), "/system/xbin/busybox", 755);
                Message msg = new Message();
                msg.what = ret ? 1 : 0;
                hEnvReady.sendMessage(msg);
            }
        }).start();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_clean;
    }

    @Override
    public String getMainActivityName() {
        return null;
    }

    @Override
    public void initMenu(Menu menu) {
        menu.clear();
        miRun = menu.add(0, 1, 1, R.string.ab_clean);
        miRun.setIcon(android.R.drawable.ic_menu_delete);
        miRun.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case 1:
                threadClean();
                break;
        }
        return true;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        Bundle bn = new Bundle();
        bn.putBoolean("isCleaning", isCleaning);
        return bn;
    }

    private void threadClean() {
        tvClean.append(getString(R.string.view_start_clean));
        miRun.setEnabled(false);
        isCleaning = true;
        new Thread(new Runnable() {
            @Override
            public void run() {
                NativeAPI.systemClean(getContext());
            }
        }).start();
    }

    private Handler hCallback = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            Intent inCallback = (Intent) msg.obj;
            int status = inCallback.getIntExtra(KEY_STATUS, -1);
            String data = inCallback.getStringExtra(KEY_DATA);
            if (status == NativeAPI.STATUS_PROGRESS || status == NativeAPI.STATUS_ERROR) {
                tvClean.append(data + "\n");
            } else if (status == NativeAPI.STATUS_COMPLETE) {
                tvClean.append(data + "\n");
                isCleaning = false;
                miRun.setEnabled(true);
            }
            svClean.fullScroll(ScrollView.FOCUS_DOWN);
            super.handleMessage(msg);
        }
    };

    private class CleanCallbackReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Message msg = new Message();
            msg.obj = intent;
            hCallback.sendMessage(msg);
        }
    }
}
