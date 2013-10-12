package com.rarnu.ucloud.android;

import android.app.Fragment;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.ucloud.android.fragment.UserFragment;

public class UserActivity extends BaseActivity {
    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        return new UserFragment();
    }
}
