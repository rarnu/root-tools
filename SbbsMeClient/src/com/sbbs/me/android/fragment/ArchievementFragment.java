package com.sbbs.me.android.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;

public class ArchievementFragment extends BaseFragment {

	public ArchievementFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_archievement_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_archievement;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_archievement;
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
		return R.layout.fragment_archievement;
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
