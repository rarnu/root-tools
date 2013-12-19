package com.yugioh.android;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.yugioh.android.fragments.CardInfoFragment;

public class CardInfoActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.icon;
	}

	@Override
	public Fragment replaceFragment() {
		return new CardInfoFragment();
	}

    @Override
    public int customTheme() {
        return 0;
    }
}
