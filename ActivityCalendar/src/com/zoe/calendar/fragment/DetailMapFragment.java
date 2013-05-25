package com.zoe.calendar.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.ActivityItem;

public class DetailMapFragment extends BaseFragment {

	ActivityItem actItem;

	public DetailMapFragment(String tag, String title) {
		super(tag, title);
	}

	@Override
	public int getBarTitle() {
		return R.string.detail_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.detail_name;
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
		actItem = (ActivityItem) getActivity().getIntent()
				.getSerializableExtra("item");

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_detail_map;
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
