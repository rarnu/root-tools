package com.zoe.calendar.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.zoe.calendar.R;
import com.zoe.calendar.common.Config;

public class RightMotionFragment extends BaseFragment {

	// TODO: motion
	
	private int[] motions;

	public RightMotionFragment(String tag) {
		super(tag, "");
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
		motions = new int[10];
		for (int i = 0; i < motions.length; i++) {
			motions[i] = Config.getMotion(getActivity(), i);
		}

	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_right_motion;
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
