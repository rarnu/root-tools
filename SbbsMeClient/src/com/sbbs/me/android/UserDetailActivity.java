package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.UserDetailFragment;
import com.sbbs.me.android.utils.CustomUIUtils;

public class UserDetailActivity extends BaseActivity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		CustomUIUtils.customActionBarHome(bar);
	}

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		return new UserDetailFragment();
	}

}
