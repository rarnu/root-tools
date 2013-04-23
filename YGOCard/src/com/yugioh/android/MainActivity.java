package com.yugioh.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.base.inner.InnerFragment;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.devlib.utils.UIUtils;
import com.yugioh.android.fragments.DuelToolFragment;
import com.yugioh.android.fragments.LeftMenuFragment;
import com.yugioh.android.fragments.LimitFragment;
import com.yugioh.android.fragments.MainFragment;
import com.yugioh.android.fragments.NewCardFragment;
import com.yugioh.android.fragments.RightMenuFragment;
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

	}

	@Override
	public void releaseFragments() {

	}

	@Override
	public Fragment replaceMenuFragment() {
		return new LeftMenuFragment(getString(R.tag.tag_menu_left), "");
	}

	@Override
	public Fragment replaceSecondMenuFragment() {
		return new RightMenuFragment(getString(R.tag.tag_menu_right), "");
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
		return new MainFragment(getString(R.tag.tag_main), "");
	}

	@Override
	public void switchPage(int page) {
		if (currentPage != page) {
			currentPage = page;
			Fragment f = getCurrentFragment(currentPage);
			if (!f.isAdded()) {
				getFragmentManager()
						.beginTransaction()
						.replace(R.id.fReplacement, f,
								((InnerFragment) f).getTagText()).commit();
			}
		}
		toggle();
	}

	private Fragment getCurrentFragment(int page) {
		Fragment f = null;
		switch (page) {
		case 0:
			// MAIN
			f = new MainFragment(getString(R.tag.tag_main), "");
			break;
		case 1:
			// LIMIT
			f = new LimitFragment(getString(R.tag.tag_main_limit), "");
			break;
		case 2:
			// NEW CARD
			f = new NewCardFragment(getString(R.tag.tag_main_newcard), "");
			break;
		case 3:
			// DUEL TOOL
			f = new DuelToolFragment(getString(R.tag.tag_main_dueltool), "");
			break;
		}
		return f;
	}
}
