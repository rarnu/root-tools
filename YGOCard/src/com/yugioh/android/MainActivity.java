package com.yugioh.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.devlib.utils.UIUtils;
import com.yugioh.android.global.FragmentNames;
import com.yugioh.android.global.Fragments;
import com.yugioh.android.intf.IMainIntf;

public class MainActivity extends BaseSlidingActivity implements IMainIntf {

	int currentPage = 0;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
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

	@Override
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
		case 2:
			// NEW CARD
			f = Fragments.getFragment(this, FragmentNames.FRAGMENT_NEWCARD);
			break;
		case 3:
			// DUEL TOOL
			f = Fragments.getFragment(this, FragmentNames.FRAGMENT_DUELTOOL);
			break;
		}
		return f;
	}


}
