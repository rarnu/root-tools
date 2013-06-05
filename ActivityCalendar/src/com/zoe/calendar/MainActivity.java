package com.zoe.calendar;

import java.io.File;

import android.app.Fragment;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Bitmap.CompressFormat;
import android.graphics.BitmapFactory;
import android.os.Bundle;

import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.classes.UpdateInfo;
import com.zoe.calendar.common.Config;
import com.zoe.calendar.dialog.UpdateDialog;
import com.zoe.calendar.fragment.LeftMenuFragment;
import com.zoe.calendar.fragment.MainFragment;
import com.zoe.calendar.utils.APIUtils;
import com.zoe.calendar.utils.APIUtils.UpdateCallback;

public class MainActivity extends BaseSlidingActivity implements UpdateCallback {

	@Override
	public void onCreate(Bundle savedInstanceState) {
		Global.synced = false;
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);

		super.onCreate(savedInstanceState);

		Global.city = Config.getCity(this);
		Global.city_pinyin = Config.getCityPinyin(this);

		if (Global.city.equals("")) {
			startActivityForResult(new Intent(this, CityActivity.class), 0);
		}

		if (Config.getIsFirstStart(this)) {
			Config.setIsFirstStart(this, false);
			startActivity(new Intent(this, NewbieActivity.class));
		}

		Global.filteredTagsString = Config.loadFiltedString(this);

		for (int i = 0; i < Global.settingTypes.length; i++) {
			Global.settingTypes[i] = Config.getSettingType(this, i);
		}

		extractIconT();
		initUpdate();

	}

	@Override
	protected void onDestroy() {
		Global.synced = false;
		super.onDestroy();
	}

	private void initUpdate() {
		APIUtils.checkUpdate(this, this);
	}

	private void extractIconT() {
		Global.iconFilePath = "/data/data/" + getPackageName()
				+ "/files/icon.png";
		if (!new File(Global.iconFilePath).exists()) {
			new Thread(new Runnable() {

				@Override
				public void run() {
					Bitmap eIco = BitmapFactory.decodeResource(getResources(),
							R.drawable.ic_launcher);

					ImageUtils.saveBitmapToFile(eIco, Global.iconFilePath,
							CompressFormat.PNG);

					RootUtils.runCommand("chmod 644 " + Global.iconFilePath,
							false);

				}
			}).start();
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
		return R.drawable.ic_logo;
	}

	@Override
	public Fragment replaceFragment() {
		return new MainFragment(getString(R.tag.fragment_main));
	}

	@Override
	public void onUpdateFound(UpdateInfo update) {
		if (update != null) {
			startActivity(new Intent(this, UpdateDialog.class).putExtra("url",
					update.url));
		}
	}

}
