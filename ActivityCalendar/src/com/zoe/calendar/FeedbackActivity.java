package com.zoe.calendar;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.zoe.calendar.fragment.FeedbackFragment;

public class FeedbackActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.ic_logo;
	}

	@Override
	public Fragment replaceFragment() {
		return new FeedbackFragment(getString(R.tag.fragment_feedback));
	}

}
