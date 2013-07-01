package com.sbbs.me.android;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseDialog;
import com.sbbs.me.android.fragment.SelectLoginFragment;

public class SelectLoginActivity extends BaseDialog {

	@Override
	public Fragment replaceFragment() {
		return new SelectLoginFragment();
	}

	@Override
	public boolean getCondition() {
		return false;
	}

}
