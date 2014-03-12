package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import com.rarnu.devlib.base.BaseDialog;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.common.FragmentNameConst;

public class BuildPropEditActivity extends BaseDialog {

    @Override
    public boolean getCondition() {
        return false;
    }

    @Override
    public Fragment replaceFragment() {
        return Fragments.getFragment(FragmentNameConst.FN_BUILD_PROP_EDIT);
    }

    @Override
    public int customTheme() {
        return 0;
    }
}
