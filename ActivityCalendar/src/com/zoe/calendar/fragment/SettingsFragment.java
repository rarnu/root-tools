package com.zoe.calendar.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.zoe.calendar.R;

public class SettingsFragment extends BaseFragment {

	public SettingsFragment(String tag) {
		super(tag, "");
	}
	
	@Override
	public int getBarTitle() {
		return R.string.settings_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.settings_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {

	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_settings;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}


}
