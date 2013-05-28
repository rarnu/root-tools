package com.zoe.calendar;

import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.classes.UpdateInfo;
import com.zoe.calendar.common.Config;
import com.zoe.calendar.dialog.UpdateDialog;
import com.zoe.calendar.fragment.LeftMenuFragment;
import com.zoe.calendar.fragment.MainFragment;
import com.zoe.calendar.fragment.RightMotionFragment;
import com.zoe.calendar.utils.APIUtils;
import com.zoe.calendar.utils.APIUtils.UpdateCallback;

public class MainActivity extends BaseSlidingActivity implements UpdateCallback {

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
		super.onCreate(savedInstanceState);
		APIUtils.init(this);
		Global.city = Config.getCity(this);
		Global.city_pinyin = Config.getCityPinyin(this);

		for (int i = 0; i < Global.settingTypes.length; i++) {
			Global.settingTypes[i] = Config.getSettingType(this, i);
		}

		if (Global.city.equals("")) {
			startActivityForResult(new Intent(this, CityActivity.class), 0);
		}

		initUpdate();
	}

	

	private void initUpdate() {
		APIUtils.checkUpdate(this, this);
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
		return new RightMotionFragment(getString(R.tag.fragment_right_motion));
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
		return R.drawable.ic_launcher;
	}

	@Override
	public Fragment replaceFragment() {
		return new MainFragment(getString(R.tag.fragment_main));
	}

	@Override
	public void onUpdateFound(UpdateInfo update) {
		if (update != null) {
			// TODO update.url;
			startActivity(new Intent(this, UpdateDialog.class));
		}

	}

}
