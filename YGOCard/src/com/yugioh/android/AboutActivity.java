package com.yugioh.android;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseDialog;
import com.yugioh.android.fragments.AboutFragment;

public class AboutActivity extends BaseDialog {

	@Override
	public boolean getCondition() {
		return false;
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
