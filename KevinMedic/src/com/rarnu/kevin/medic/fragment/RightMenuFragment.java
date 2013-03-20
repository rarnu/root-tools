package com.rarnu.kevin.medic.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.kevin.medic.MainActivity;
import com.rarnu.kevin.medic.R;

public class RightMenuFragment extends BaseFragment implements OnClickListener {

	RelativeLayout layHeadBar;
	ImageView iSettings;

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
		layHeadBar = (RelativeLayout) innerView.findViewById(R.id.layHeadBar);
		iSettings = (ImageView) innerView.findViewById(R.id.iSettings);
	}

	@Override
	protected void initEvents() {
		iSettings.setOnClickListener(this);
		layHeadBar.setOnClickListener(this);
	}

	@Override
	protected void initLogic() {

	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_menu_right;
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.iSettings:
			Toast.makeText(getActivity(), R.string.app_settings,
					Toast.LENGTH_SHORT).show();
			break;
		case R.id.layHeadBar:
			((MainActivity) getActivity()).getSlidingMenu().toggle();
			break;
		}

	}

}
