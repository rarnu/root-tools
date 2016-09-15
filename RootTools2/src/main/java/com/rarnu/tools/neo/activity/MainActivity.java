package com.rarnu.tools.neo.activity;

import android.Manifest;
import android.app.AlertDialog;
import android.app.Fragment;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.api.NativeAPI;
import com.rarnu.tools.neo.base.BaseActivity;
import com.rarnu.tools.neo.fragment.MainFragment;
import com.rarnu.tools.neo.utils.UIUtils;
import com.rarnu.tools.neo.xposed.XpStatus;

public class MainActivity extends BaseActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        UIUtils.initDisplayMetrics(this, getWindowManager(), false);
        super.onCreate(savedInstanceState);
        NativeAPI.isRejected = !NativeAPI.mount();

        if (!XpStatus.isEnable()) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.alert_hint)
                    .setMessage(R.string.alert_xposed)
                    .setCancelable(false)
                    .setPositiveButton(R.string.alert_ok, null)
                    .show();
        }
        if (Build.VERSION.SDK_INT >= 24) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.alert_hint)
                    .setMessage(R.string.alert_androidn_pending)
                    .setPositiveButton(R.string.alert_ok, null)
                    .show();
        } else {
            if (NativeAPI.isRejected) {
                new AlertDialog.Builder(this)
                        .setTitle(R.string.alert_hint)
                        .setMessage(R.string.alert_root)
                        .setCancelable(false)
                        .setPositiveButton(R.string.alert_ok, null)
                        .show();
            }
        }
        requirePermission();
    }

    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        return new MainFragment();
    }

    @Override
    public int customTheme() {
        return 0;
    }

    @Override
    public boolean getActionBarCanBack() {
        return false;
    }

    private void requirePermission() {
        if (Build.VERSION.SDK_INT >= 23) {
            if (checkSelfPermission(Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE, Manifest.permission.READ_EXTERNAL_STORAGE}, 0);
            } else {
                XpStatus.canWriteSdcard = true;
            }
        } else {
            XpStatus.canWriteSdcard = true;
        }
    }

    // No override here for compact with 5.0
    // @Override
    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        for (int i = 0; i < permissions.length; i++) {
            if (permissions[i].equals(Manifest.permission.WRITE_EXTERNAL_STORAGE)) {
                XpStatus.canWriteSdcard = grantResults[i] == PackageManager.PERMISSION_GRANTED;
                break;
            }
        }
    }
}
