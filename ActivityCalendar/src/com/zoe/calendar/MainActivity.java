package com.zoe.calendar;

import android.app.ActionBar;
import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.animation.RotateAnimation;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.classes.CityCodeItem;
import com.zoe.calendar.classes.UpdateInfo;
import com.zoe.calendar.classes.WeatherInfo;
import com.zoe.calendar.common.Config;
import com.zoe.calendar.fragment.LeftMenuFragment;
import com.zoe.calendar.fragment.MainFragment;
import com.zoe.calendar.fragment.RightMotionFragment;
import com.zoe.calendar.utils.APIUtils;
import com.zoe.calendar.utils.APIUtils.UpdateCallback;
import com.zoe.calendar.utils.APIUtils.WeatherCallback;
import com.zoe.calendar.utils.AnimateUtils;
import com.zoe.calendar.utils.CityUtils;

public class MainActivity extends BaseSlidingActivity implements
		WeatherCallback, OnClickListener, UpdateCallback {

	ActionBar bar;
	View actionBarView;
	RelativeLayout layWeather;
	ImageView ivWeather;
	TextView tvTemp;
	Button btnSync;
	RotateAnimation animSync;
	
	WeatherInfo weather;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
		super.onCreate(savedInstanceState);
		Global.city = Config.getCity(this);
		Global.city_pinyin = Config.getCityPinyin(this);
		Global.dataTimestamp = Config.getLastTimestamp(this);

		for (int i = 0; i < Global.settingTypes.length; i++) {
			Global.settingTypes[i] = Config.getSettingType(this, i);
		}

		if (Global.city.equals("")) {
			startActivityForResult(new Intent(this, CityActivity.class), 0);
		}

		initActionBar();
		initWeather();
		initUpdate();
	}

	private void initActionBar() {
		bar = getActionBar();
		actionBarView = getLayoutInflater().inflate(R.layout.actionbar_custom,
				null);
		ActionBar.LayoutParams alp = new ActionBar.LayoutParams(
				ActionBar.LayoutParams.WRAP_CONTENT,
				ActionBar.LayoutParams.WRAP_CONTENT);
		alp.gravity = Gravity.END;
		int flags = ActionBar.DISPLAY_SHOW_CUSTOM;
		int change = bar.getDisplayOptions() ^ flags;
		bar.setCustomView(actionBarView, alp);
		bar.setDisplayOptions(change, flags);
		
		layWeather = (RelativeLayout) actionBarView.findViewById(R.id.layWeather);
		ivWeather = (ImageView) actionBarView.findViewById(R.id.ivWeather);
		tvTemp = (TextView) actionBarView.findViewById(R.id.tvTemp);
		btnSync = (Button) actionBarView.findViewById(R.id.btnSync);
		
		layWeather.setOnClickListener(this);
		btnSync.setOnClickListener(this);
		
		animSync = AnimateUtils.getRotateAnimation();
		
		btnSync.setAnimation(animSync);
		animSync.start();

	}

	private void initWeather() {
		if (Global.city == null || Global.city.equals("")) {
			return;
		}

		final Handler hWeather = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					APIUtils.getWeather(msg.arg1, MainActivity.this);
				}
				super.handleMessage(msg);
			}
		};
		new Thread(new Runnable() {

			@Override
			public void run() {
				CityUtils.loadCityCode(MainActivity.this);
				CityCodeItem item = CityUtils.findCityCode(Global.city);
				if (item != null) {
					Message msg = new Message();
					msg.what = 1;
					msg.arg1 = item.code;
					hWeather.sendMessage(msg);
				}
			}
		}).start();

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

		initWeather();
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
	public void onGetWeather(WeatherInfo weather) {
		this.weather = weather;
		tvTemp.setText(weather.temp);
		// TODO: convert weather to image

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnSync:
			// TODO: sync and merge data
			break;
		case R.id.layWeather:
			// TODO: show weather info
			break;
		}
		
	}

	@Override
	public void onUpdateFound(UpdateInfo update) {
		if (update != null) {
			// TODO: update
		}
		
	}

}
