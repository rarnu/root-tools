package com.yugioh.android;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.yugioh.android.fragments.SettingsFragment;

public class SettingsActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.icon;
	}

	@Override
	public Fragment replaceFragment() {
		return new SettingsFragment();
	}

    @Override
    public int customTheme() {
        return 0;
    }

}
