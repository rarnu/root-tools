package com.rarnu.tools.neo.activity;

import android.app.Fragment;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseActivity;
import com.rarnu.tools.neo.fragment.ComponentFragment;

public class ComponentActivity extends BaseActivity {
    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        return new ComponentFragment();
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
