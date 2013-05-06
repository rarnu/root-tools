package com.yugioh.android.fragments;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;

public class SettingsFragment extends BaseFragment {

	// TODO: font settings
	// TODO: fitable
	
	@Override
	public int getBarTitle() {
		return R.string.settings;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.settings;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	protected void initComponents() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void initEvents() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void initLogic() {
		// TODO Auto-generated method stub

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_settings;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

}
