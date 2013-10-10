package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import android.content.res.Configuration;
import android.os.Bundle;
import com.rarnu.devlib.base.BaseDialog;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.common.FragmentNameConst;

public class PoolActivity extends BaseDialog {

    private static int screenState1 = -1, screenState2 = -1;

    @Override
    protected void onCreate(Bundle savedInstanceState) {

        super.onCreate(savedInstanceState);
        if (screenState1 == -1 && screenState2 == -1) {
            screenState1 = screenState2 = getResources().getConfiguration().orientation;
        } else {
            screenState1 = screenState2;
            screenState2 = getResources().getConfiguration().orientation;
        }
        if (screenState1 == Configuration.ORIENTATION_LANDSCAPE && screenState2 == Configuration.ORIENTATION_PORTRAIT) {
            screenState1 = screenState2 = -1;
            finish();
            return;
        }
    }

    @Override
    public boolean getCondition() {
        return false;
    }

    @Override
    public Fragment replaceFragment() {
        return Fragments.getFragment(FragmentNameConst.FN_POOL);
    }
}
