package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;
import android.view.KeyEvent;
import com.rarnu.devlib.base.BasePopupActivity;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;

public class SelectSendFileActivity extends BasePopupActivity {

    BasePopupFragment fragmentSelectSendFile;

    @Override
    public Fragment replaceFragment() {
        fragmentSelectSendFile = (BasePopupFragment) Fragments.getFragment(FragmentNameConst.FN_SELECT_SEND_FILE);
        return fragmentSelectSendFile;
    }

    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (fragmentSelectSendFile != null) {
                fragmentSelectSendFile.setNewArguments(null);
                return true;
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public int customTheme() {
        return GlobalInstance.theme ? android.R.style.Theme_Holo_Light : android.R.style.Theme_Holo;
    }

}
