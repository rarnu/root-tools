package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import android.os.Bundle;
import com.rarnu.devlib.base.BaseDialog;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.common.FragmentNameConst;

public class InstallApkActivity extends BaseDialog {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setFinishOnTouchOutside(false);
    }

    @Override
    public boolean getCondition() {
        return false;
    }

    @Override
    public Fragment replaceFragment() {
        return Fragments.getFragment(FragmentNameConst.FN_INSTALL_APK);
    }
}
