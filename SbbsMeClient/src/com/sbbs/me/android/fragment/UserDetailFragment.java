package com.sbbs.me.android.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;

public class UserDetailFragment extends BaseFragment {

	public UserDetailFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_user_detail_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.userdetail_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.userdetail_name;
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
		return R.layout.fragment_user_detail;
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
