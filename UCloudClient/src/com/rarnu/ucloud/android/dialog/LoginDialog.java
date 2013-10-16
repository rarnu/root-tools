package com.rarnu.ucloud.android.dialog;

import android.app.Fragment;
import com.rarnu.devlib.base.BaseDialog;
import com.rarnu.ucloud.android.fragment.LoginFragment;

public class LoginDialog extends BaseDialog {
    @Override
    public boolean getCondition() {
        return false;
    }

    @Override
    public Fragment replaceFragment() {
        return new LoginFragment();
    }
}
