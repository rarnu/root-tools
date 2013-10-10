package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import android.view.KeyEvent;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BasePopupActivity;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;

public class SysappSelectApkActivity extends BasePopupActivity {

    BasePopupFragment fragmentSysappSelectApk;

    @Override
    public Fragment replaceFragment() {
        fragmentSysappSelectApk = (BasePopupFragment) Fragments.getFragment(FragmentNameConst.FN_SYSAPP_SELECTAPK);
        return fragmentSysappSelectApk;
    }

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (fragmentSysappSelectApk != null) {
                fragmentSysappSelectApk.setNewArguments(null);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }
}
