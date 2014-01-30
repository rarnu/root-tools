package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import com.rarnu.devlib.base.BasePopupActivity;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;

public class PasswordDetailActivity extends BasePopupActivity {
    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        return Fragments.getFragment(FragmentNameConst.FN_PASSWORD_DETAIL);
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme ? android.R.style.Theme_Holo_Light : android.R.style.Theme_Holo;
    }
}
