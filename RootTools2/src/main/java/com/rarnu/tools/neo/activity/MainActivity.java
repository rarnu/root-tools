package com.rarnu.tools.neo.activity;

import android.Manifest;
import android.app.Fragment;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.api.DeviceAPI;
import com.rarnu.tools.neo.base.BaseActivity;
import com.rarnu.tools.neo.fragment.MainFragment;
import com.rarnu.tools.neo.utils.AppUtils;
import com.rarnu.tools.neo.utils.UIUtils;
import com.rarnu.tools.neo.xposed.XpStatus;

public class MainActivity extends BaseActivity {

    private SharedPreferences pref = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        UIUtils.initDisplayMetrics(this, getWindowManager(), false);
        super.onCreate(savedInstanceState);
        pref = getSharedPreferences(XpStatus.PREF, Build.VERSION.SDK_INT < 24 ? 1 : 0);
        XpStatus.mode = pref.getBoolean(XpStatus.KEY_WORK_MODE, false) ? XpStatus.Mode.NDK : XpStatus.Mode.JVM;

        DeviceAPI.setRejected(!DeviceAPI.mount());
        DeviceAPI.setSystemRW(DeviceAPI.isSystemRW());
        boolean isRooted = !DeviceAPI.isRejected();
        Log.e("DeviceAPI", "isRejected => " + DeviceAPI.isRejected());
        Log.e("DeviceAPI", "isSystemRW => " + DeviceAPI.isSystemRW());

        if (!DeviceAPI.isSystemRW()) {
            Toast.makeText(this, R.string.toast_need_crack_system, Toast.LENGTH_SHORT).show();
        }

        boolean xpEnabled = XpStatus.isEnable();

        if (!xpEnabled && !isRooted) {
            Toast.makeText(this, R.string.toast_need_root_xposed, Toast.LENGTH_SHORT).show();
        } else if (!xpEnabled && isRooted) {
            Toast.makeText(this, R.string.toast_need_xposed, Toast.LENGTH_SHORT).show();
        } else if (xpEnabled && !isRooted) {
            Toast.makeText(this, R.string.toast_need_root, Toast.LENGTH_SHORT).show();
        }

        AppUtils.doScanMedia(this);
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

    @SuppressWarnings("Duplicates")
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
