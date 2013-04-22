package com.yugioh.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.devlib.utils.UIUtils;

public class MainActivity extends BaseSlidingActivity {

	int currentPage = 0;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager());
		super.onCreate(savedInstanceState);
	}

	@Override
	public void loadFragments() {
		Fragments.Load(this);
	}

	@Override
	public void releaseFragments() {
		Fragments.Release();
	}

	@Override
	public Fragment replaceMenuFragment() {
		return Fragments.getFragment(this, FragmentNames.FRAGMENT_LEFTMENU);
	}

	@Override
	public Fragment replaceSecondMenuFragment() {
		return Fragments.getFragment(this, FragmentNames.FRAGMENT_RIGHTMENU);
	}

	@Override
	public int getBehindOffset() {
		return UIUtils.dipToPx(200);
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
		return SlidingMenu.LEFT_RIGHT;
	}

	@Override
	public int getIcon() {
		return R.drawable.icon;
	}

	@Override
	public Fragment replaceFragment() {
		return Fragments.getFragment(this, FragmentNames.FRAGMENT_MAIN);
	}

	public void switchPage(int page) {
		currentPage = page;
		Fragment f = getCurrentFragment(currentPage);
		if (!f.isAdded()) {
			getFragmentManager().beginTransaction()
					.replace(R.id.fReplacement, f).commit();
		}
		toggle();
	}

	private Fragment getCurrentFragment(int page) {
		Fragment f = null;
		switch (page) {
		case 0:
			// MAIN
			f = Fragments.getFragment(this, FragmentNames.FRAGMENT_MAIN);
			break;
		case 1:
			// LIMIT
			f = Fragments.getFragment(this, FragmentNames.FRAGMENT_LIMIT);
			break;
		}
		return f;
	}

}
