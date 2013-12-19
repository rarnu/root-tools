package com.yugioh.android;

import android.app.Fragment;
import com.rarnu.devlib.base.BaseActivity;
import com.yugioh.android.fragments.PackageCardsFragment;

public class PackageCardsActivity extends BaseActivity {
    @Override
    public int getIcon() {
        return R.drawable.icon;
    }

    @Override
    public Fragment replaceFragment() {
        PackageCardsFragment pcf = new PackageCardsFragment();
        pcf.setArguments(getIntent().getExtras());
        return pcf;
    }

    @Override
    public int customTheme() {
        return 0;
    }
}
