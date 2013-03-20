package com.rarnu.kevin.medic.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.kevin.medic.MainActivity;
import com.rarnu.kevin.medic.R;

public class Page3Fragment extends BaseFragment {

	@Override
	protected int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	protected void initComponents() {


	}

	@Override
	protected void initEvents() {


	}

	@Override
	protected void initLogic() {
		

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_page_1;
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void initMenu(Menu menu) {


	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

}
