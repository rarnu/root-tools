package com.sbbs.me.android;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.AboutFragment;

public class AboutActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		return new AboutFragment();
	}

    @Override
    public int customTheme() {
        return 0;
    }

}
