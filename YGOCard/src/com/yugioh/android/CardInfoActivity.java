package com.yugioh.android;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;

public class CardInfoActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.icon;
	}

	@Override
	public Fragment replaceFragment() {
		return Fragments.getFragment(this, FragmentNames.FRAGMENT_CARDINFO);
	}

}
