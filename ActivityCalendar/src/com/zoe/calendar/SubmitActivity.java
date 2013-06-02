package com.zoe.calendar;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.zoe.calendar.fragment.SubmitFragment;

public class SubmitActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.ic_logo;
	}

	@Override
	public Fragment replaceFragment() {
		return new SubmitFragment(getString(R.tag.fragment_submit));
	}

}
