package com.rarnu.tools.neo.activity;

import android.app.AlertDialog;
import android.app.Fragment;
import android.os.Bundle;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseActivity;
import com.rarnu.tools.neo.fragment.MainFragment;
import com.rarnu.tools.neo.root.RootUtils;
import com.rarnu.tools.neo.utils.UIUtils;
import com.rarnu.tools.neo.xposed.XpStatus;

public class MainActivity extends BaseActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        UIUtils.initDisplayMetrics(this, getWindowManager(), false);
        super.onCreate(savedInstanceState);
        RootUtils.mountRW();

        if (!XpStatus.isEnable()) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.alert_hint)
                    .setMessage(R.string.alert_xposed)
                    .setCancelable(false)
                    .setPositiveButton(R.string.alert_ok, null)
                    .show();
        }
        if (RootUtils.isRejected()) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.alert_hint)
                    .setMessage(R.string.alert_root)
                    .setCancelable(false)
                    .setPositiveButton(R.string.alert_ok, null)
                    .show();
        }
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
}
