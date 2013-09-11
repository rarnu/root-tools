package com.rarnu.adcenter;

import android.app.Fragment;

import com.rarnu.adcenter.fragment.RegisterFragment;
import com.rarnu.devlib.base.BaseActivity;

public class RegisterActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.ic_launcher;
	}

	@Override
	public Fragment replaceFragment() {
		return new RegisterFragment();
	}

}
