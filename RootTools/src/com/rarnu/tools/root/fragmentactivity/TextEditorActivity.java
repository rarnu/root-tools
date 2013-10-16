package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import com.rarnu.devlib.base.BasePopupActivity;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;

public class TextEditorActivity extends BasePopupActivity {

    @Override
    public boolean getCloseCondition() {
        return false;
    }

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        return Fragments.getFragment(FragmentNameConst.FN_TEXT_EDITOR);
    }
}
