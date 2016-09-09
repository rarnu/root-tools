package com.rarnu.tools.neo.fragment;

import android.os.*;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.root.CommandResult;
import com.rarnu.tools.neo.root.RootUtils;
import com.rarnu.tools.neo.utils.FileUtils;

import java.io.File;

public class CleanFragment extends BaseFragment {


    private TextView tvClean = null;
    private MenuItem miRun = null;
    private boolean isCleaning = false;

    private String duCmd = "";

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
        tvClean = (TextView) innerView.findViewById(R.id.tvClean);
    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {
        boolean busyboxExists = new File("/system/bin/busybox").exists() || new File("/system/xbin/busybox").exists();
        boolean duExists = new File("/system/bin/du").exists() || new File("/system/xbin/du").exists();
        if (duExists) {
            duCmd = "du";
            tvClean.setText(R.string.view_ready);
            return;
        }
        duCmd = "busybox du";
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
                RootUtils.mountRW();
                CommandResult ret = RootUtils.runCommand(new String[] {
                        String.format("cat %s > /system/xbin/busybox", fBusybox.getAbsolutePath()),
                        "chmod 755 /system/xbin/busybox"
                }, true);
                Message msg = new Message();
                msg.what = ret.error.equals("") ? 1 : 0;
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

    private Handler hInfo = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            tvClean.append((String) msg.obj);
            super.handleMessage(msg);
        }
    };

    private Handler hComplete = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            isCleaning = false;
            miRun.setEnabled(true);
            super.handleMessage(msg);
        }
    };

    private void sendMessageStr(String str) {
        Message msg = new Message();
        msg.obj = str;
        hInfo.sendMessage(msg);
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
                long totalSize = 0L; // K
                // clean app cache
                CommandResult ret = RootUtils.runCommand("find /data/data/ -type dir -name \"cache\"", true);
                String[] items = ret.result.split("\n");
                CacheSize cs;
                for (String s: items) {
                    cs = getSize(s);
                    if (cs.size > 16) { // clean only above 16K
                        if (deleteCache(s)) {
                            sendMessageStr(getString(R.string.view_clean_cache, s, cs.sizeReadable));
                            totalSize += cs.size;
                        }
                    }
                }
                // clean anr log
                CacheSize anrSize = getSize("/data/anr/");
                if (deleteAnrLog()) {
                    sendMessageStr(getString(R.string.view_clean_anr, anrSize.sizeReadable));
                    totalSize += anrSize.size;
                }
                // clean art
                totalSize += deleteRemainArtCache();
                sendMessageStr(getString(R.string.view_clean_complete, FileUtils.getReadableFileSize(totalSize)));
                hComplete.sendEmptyMessage(0);
            }
        }).start();
    }

    private CacheSize getSize(String path) {
        CommandResult ret = RootUtils.runCommand(String.format("%s -s -k \"%s\"", duCmd, path), true);
        String sizeStr = "0";
        long size = 0L;
        try {
            sizeStr = ret.result.substring(0, ret.result.indexOf('\t')).trim();
            size = Long.parseLong(sizeStr);
        } catch (Exception e) {
            Log.e("CleanFragment", "getSize => error: " + ret.error);
        }
        return new CacheSize(sizeStr + "K", size);
    }

    private boolean deleteCache(String path) {
        CommandResult ret = RootUtils.runCommand(String.format("rm -r \"%s\"", path), true);
        return ret.error.equals("");
    }

    private boolean deleteAnrLog() {
        CommandResult ret = RootUtils.runCommand("rm -r /data/anr/*", true);
        return ret.error.equals("");
    }

    private long deleteRemainArtCache() {

        CommandResult list = RootUtils.runCommand("ls /data/app", true);
        String[] listInstalled = list.result.split("\n");
        CommandResult listAll = RootUtils.runCommand("pm list packages", true);
        String[] listAllInstalled = listAll.result.split("\n");
        CommandResult retArm = RootUtils.runCommand("ls /data/dalvik-cache/arm", true);
        CommandResult retArm64 = RootUtils.runCommand("ls /data/dalvik-cache/arm64", true);
        CommandResult retProfile = RootUtils.runCommand("ls /data/dalvik-cache/profiles", true);
        String[] listArm = retArm.result.split("\n");
        String[] listArm64 = retArm64.result.split("\n");
        String[] listProfile = retProfile.result.split("\n");
        long totalSize = 0L;
        String tmpPath;
        CacheSize size;
        for (String s : listArm) {
            if (!s.trim().equals("")) {
                if (!isCachedAppInstalled(listInstalled, s)) {
                    tmpPath = "/data/dalvik-cache/arm/" + s;
                    size = getSize(tmpPath);
                    if (deleteCache(tmpPath)) {
                        sendMessageStr(getString(R.string.view_clean_art_remain, s, size.sizeReadable));
                        totalSize += size.size;
                    }
                }
            }
        }

        for (String s: listArm64) {
            if (!s.trim().equals("")) {
                if (!isCachedAppInstalled(listInstalled, s)) {
                    tmpPath = "/data/dalvik-cache/arm64/" + s;
                    size = getSize(tmpPath);
                    if (deleteCache(tmpPath)) {
                        sendMessageStr(getString(R.string.view_clean_art_remain, s, size.sizeReadable));
                        totalSize += size.size;
                    }
                }
            }
        }

        for (String s: listProfile) {
            if (!s.trim().equals("")) {
                if (!isProfileInstalled(listAllInstalled, s)) {
                    tmpPath = "/data/dalvik-cache/profiles/" + s;
                    size = getSize(tmpPath);
                    if (deleteCache(tmpPath)) {
                        sendMessageStr(getString(R.string.view_clean_art_remain, s, size.sizeReadable));
                        totalSize += size.size;
                    }
                }
            }
        }
        return totalSize;
    }

    private boolean isCachedAppInstalled(String[] oriList, String app) {
        if (app.startsWith("system") || app.startsWith("data@dalvik-cache")) {
            // do not delete anything about system
            return true;
        }
        String newAppPath = app.replace("data@app@", "");
        newAppPath = newAppPath.substring(0, newAppPath.indexOf("@"));
        boolean ret = false;
        for (String s: oriList) {
            if (s.equals(newAppPath)) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    private boolean isProfileInstalled(String[] oriList, String app) {
        boolean ret = false;
        for (String s : oriList) {
            if (s.contains(app)) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    private class CacheSize {
        String sizeReadable = "";
        long size = 0L;
        CacheSize(String sr, long s) {
            sizeReadable = sr;
            size = s;
        }
    }

}
