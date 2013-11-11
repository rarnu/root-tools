package com.rarnu.startup;

import android.app.Fragment;
import android.os.Bundle;
import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.startup.fragment.MainFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

public class MainActivity extends BaseActivity {

    MainFragment mf = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        UIUtils.initDisplayMetrics(this, getWindowManager(), false);
        ResourceUtils.init(this);
        super.onCreate(savedInstanceState);
    }

    @Override
    public int getIcon() {
        return R.drawable.ic_launcher;
    }

    @Override
    public Fragment replaceFragment() {
        if (mf == null) {
            mf = new MainFragment();
        }
        return mf;
    }
}
