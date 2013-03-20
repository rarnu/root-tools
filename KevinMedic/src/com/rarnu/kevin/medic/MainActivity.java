package com.rarnu.kevin.medic;

import android.app.Fragment;
import android.os.Bundle;
import android.view.KeyEvent;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.devlib.utils.UIUtils;

public class MainActivity extends BaseSlidingActivity {

	private int currentPage = 0;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager());
		super.onCreate(savedInstanceState);
		getActionBar().setBackgroundDrawable(getResources().getDrawable(R.color.lightgray));
	}

	@Override
	public Fragment replaceMenuFragment() {
		return Fragments.fLeftMenu;
	}

	@Override
	public Fragment replaceSecondMenuFragment() {
		return Fragments.fRightMenu;
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
		return SlidingMenu.LEFT_RIGHT;
	}

	@Override
	public int getIcon() {
		return R.drawable.ic_launcher;
	}

	@Override
	public Fragment replaceFragment() {
		return Fragments.fMain;
	}

	public void switchPage(int page) {
		currentPage = page;
		Fragment f = null;
		switch (currentPage) {
		case 0:
			f = Fragments.fMain;
			break;
		case 1:
			f = Fragments.fPage1;
			break;
		case 2:
			f = Fragments.fPage2;
			break;
		case 3:
			f = Fragments.fPage3;
			break;
		case 4:
			f = Fragments.fPage4;
			break;
		case 5:
			f = Fragments.fPage5;
			break;
		}
		if (!f.isAdded()) {
			getFragmentManager().beginTransaction()
					.replace(R.id.fReplacement, f).commit();
		}
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			if (currentPage != 0) {
				currentPage = 0;
				getFragmentManager().beginTransaction()
						.replace(R.id.fReplacement, Fragments.fMain).commit();
				return true;
			}

		}
		return super.onKeyDown(keyCode, event);
	}

	@Override
	public void loadFragments() {
		Fragments.load();
	}

	@Override
	public void releaseFragments() {
		Fragments.release();

	}
}
