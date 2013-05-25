package com.zoe.calendar;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.fragment.LeftMenuFragment;
import com.zoe.calendar.fragment.MainFragment;

public class MainActivity extends BaseSlidingActivity {

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
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
		return new LeftMenuFragment(getString(R.tag.fragment_left_menu));
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
		return SlidingMenu.TOUCHMODE_MARGIN;
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
		return new MainFragment(getString(R.tag.fragment_main));
	}

}
