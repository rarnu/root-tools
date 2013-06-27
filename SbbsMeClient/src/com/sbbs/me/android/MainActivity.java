package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;
import android.view.KeyEvent;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.fragment.LeftMenuFragment;
import com.sbbs.me.android.fragment.MainFragment;

public class MainActivity extends BaseSlidingActivity {

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
		ResourceUtils.init(this);
		super.onCreate(savedInstanceState);
	}
	
	@Override
	public void loadFragments() {
		
	}

	@Override
	public void releaseFragments() {

	}

	@Override
	public Fragment replaceMenuFragment() {
		return new LeftMenuFragment();
	}

	@Override
	public Fragment replaceSecondMenuFragment() {
		return null;
	}

	@Override
	public int getBehindOffset() {
		return UIUtils.dipToPx(150);
	}

	@Override
	public int getAboveTouchMode() {
		return SlidingMenu.TOUCHMODE_FULLSCREEN;
	}

	@Override
	public int getBehindTouchMode() {
		return SlidingMenu.TOUCHMODE_MARGIN;
	}

	@Override
	public int getSlideMode() {
		return SlidingMenu.LEFT;
	}

	@Override
	public int getIcon() {
		return R.drawable.ic_launcher;
	}

	@Override
	public Fragment replaceFragment() {
		return new MainFragment();
	}
	
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			if (getSlidingMenu().isMenuShowing()) {
				toggle();
				return true;
			}
		}
		return super.onKeyDown(keyCode, event);
	}

}
