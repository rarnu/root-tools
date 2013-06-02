package com.zoe.calendar;

import android.app.Fragment;
import android.view.KeyEvent;

import com.rarnu.devlib.base.BaseActivity;
import com.zoe.calendar.fragment.NewbieFragment;

public class NewbieActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.ic_logo;
	}

	@Override
	public Fragment replaceFragment() {
		return new NewbieFragment(getString(R.tag.fragment_newbie));
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}
}
