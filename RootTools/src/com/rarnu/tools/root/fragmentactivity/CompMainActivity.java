package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.fragment.CompContainerFragment;

public class CompMainActivity extends BaseActivity {

    CompContainerFragment fCompContainer = null;

    @Override
    public Fragment replaceFragment() {
        if (fCompContainer == null) {
            fCompContainer = (CompContainerFragment) Fragments.getFragment(FragmentNameConst.FN_COMP);
        }
        return fCompContainer;
    }

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme ? android.R.style.Theme_Holo_Light : android.R.style.Theme_Holo;
    }

}
