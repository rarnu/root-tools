package com.rarnu.adcenter;

import android.app.Fragment;

import com.rarnu.adcenter.fragment.LoginFragment;
import com.rarnu.devlib.base.BaseActivity;

public class LoginActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.ic_launcher;
	}

	@Override
	public Fragment replaceFragment() {
		return new LoginFragment();
	}

}
