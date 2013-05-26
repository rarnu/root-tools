package com.zoe.calendar;

import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.common.Config;
import com.zoe.calendar.fragment.LeftMenuFragment;
import com.zoe.calendar.fragment.MainFragment;

public class MainActivity extends BaseSlidingActivity {

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
		super.onCreate(savedInstanceState);
		Global.city = Config.getCity(this);
		Global.city_pinyin = Config.getCityPinyin(this);
		Global.dataTimestamp = Config.getLastTimestamp(this);

		if (Global.city.equals("")) {
			startActivityForResult(new Intent(this, CityActivity.class), 0);
		}

	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (Global.city.equals("")) {
			finish();
		}

		BaseFragment bfMain = (BaseFragment) (getFragmentManager()
				.findFragmentByTag(getString(R.tag.fragment_main)));
		if (bfMain != null) {
			bfMain.setNewArguments(null);
		}

		BaseFragment bfLeftMenu = (BaseFragment) (getFragmentManager()
				.findFragmentByTag(getString(R.tag.fragment_left_menu)));
		if (bfLeftMenu != null) {
			bfLeftMenu.setNewArguments(null);
		}
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
