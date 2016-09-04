package com.rarnu.tools.neo.activity;

import android.app.Fragment;
import android.content.res.Configuration;
import android.os.Bundle;
import com.rarnu.tools.neo.base.BaseDialog;
import com.rarnu.tools.neo.fragment.BuildPropEditFragment;

public class BuildPropEditActivity extends BaseDialog {

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
        return new BuildPropEditFragment();
    }

    @Override
    public int customTheme() {
        return 0;
    }
}
