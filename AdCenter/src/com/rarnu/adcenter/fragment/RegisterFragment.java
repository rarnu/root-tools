package com.rarnu.adcenter.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.adcenter.R;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;

public class RegisterFragment extends BaseFragment {

	public RegisterFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_register);
	}

	@Override
	public int getBarTitle() {
		return R.string.register;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.register;
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
		return R.layout.fragment_register;
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
