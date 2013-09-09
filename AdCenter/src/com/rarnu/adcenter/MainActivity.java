package com.rarnu.adcenter;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.adcenter.common.Config;
import com.rarnu.adcenter.fragment.CashFragment;
import com.rarnu.adcenter.fragment.HelpFragment;
import com.rarnu.adcenter.fragment.LeftMenuFragment;
import com.rarnu.adcenter.fragment.MainFragment;
import com.rarnu.adcenter.fragment.UserFragment;
import com.rarnu.adcenter.intf.Intf;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.base.inner.InnerFragment;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

public class MainActivity extends BaseSlidingActivity implements Intf {

	MainFragment mainFragment;
	LeftMenuFragment leftMenuFragment;
	CashFragment cashFragment;
	UserFragment userFragment;
	HelpFragment helpFragment;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
		ResourceUtils.init(this);
		super.onCreate(savedInstanceState);
	}

	@Override
	public void loadFragments() {
		mainFragment = new MainFragment();
		leftMenuFragment = new LeftMenuFragment();
		cashFragment = new CashFragment();
		userFragment = new UserFragment();
		helpFragment = new HelpFragment();
	}

	@Override
	public void releaseFragments() {
		mainFragment = null;
		leftMenuFragment = null;
		cashFragment = null;
		userFragment = null;
		helpFragment = null;
	}

	@Override
	public Fragment replaceMenuFragment() {
		return leftMenuFragment;
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
		Bundle bn = new Bundle();
		bn.putInt("area", Config.getCurrentArea(this));
		mainFragment.setArguments(bn);
		return mainFragment;
	}

	@Override
	public void switchPage(int index, int area) {

		Fragment f = getCurrentFragment(index);
		if (!f.isAdded()) {
			getFragmentManager()
					.beginTransaction()
					.replace(R.id.fReplacement, f,
							((InnerFragment) f).getTagText()).commit();
		}

		if (index == -1) {
			Bundle bn = new Bundle();
			bn.putInt("area", area);
			if (mainFragment.isAdded()) {
				mainFragment.setNewArguments(bn);
			} else {
				mainFragment.setArguments(bn);
			}
			Config.setCurrentArea(this, area);
		}
		getSlidingMenu().showContent();
	}

	private Fragment getCurrentFragment(int page) {
		Fragment f = null;
		switch (page) {
		case -1:
			f = mainFragment;
			break;
		case 0:
			f = userFragment;
			break;
		case 1:
			f = cashFragment;
			break;
		case 2:
			f = helpFragment;
			break;
		}
		return f;
	}

}
