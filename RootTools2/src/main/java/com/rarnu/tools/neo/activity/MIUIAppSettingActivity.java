package com.rarnu.tools.neo.activity;

import android.app.Fragment;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseActivity;
import com.rarnu.tools.neo.fragment.MIUIAppSettingFragment;

/**
 * Created by rarnu on 11/23/16.
 */
public class MIUIAppSettingActivity extends BaseActivity {
    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        return new MIUIAppSettingFragment();
    }

    @Override
    public int customTheme() {
        return 0;
    }

    @Override
    public boolean getActionBarCanBack() {
        return true;
    }
}
