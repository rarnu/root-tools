package com.cbx.android.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.cbx.android.MainActivity;
import com.cbx.android.R;
import com.cbx.android.classes.DetailItem;
import com.cbx.android.utils.ResourceUtils;
import com.rarnu.devlib.base.BaseFragment;

public class DetailFragment extends BaseFragment {

	DetailItem item = null;

	public DetailFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.fragment_detail);
	}

	@Override
	public int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.app_name;
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
		return R.layout.fragment_detail;
	}

	@Override
	public String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {
		item = (DetailItem) bn.getSerializable("item");
	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
