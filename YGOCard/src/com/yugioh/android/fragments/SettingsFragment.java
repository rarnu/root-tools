package com.yugioh.android.fragments;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.utils.UIUtils;
import com.yugioh.android.R;
import com.yugioh.android.common.Config;
import com.yugioh.android.utils.DeviceUtils;

public class SettingsFragment extends BaseFragment implements OnClickListener {

	// TODO: font settings

	private static final int[] fits = new int[] { R.drawable.c0, R.drawable.c1,
			R.drawable.c2, R.drawable.c3, R.drawable.c4, R.drawable.c5,
			R.drawable.c6, R.drawable.c7, R.drawable.c8, R.drawable.c9 };

	ImageView ivFitable;
	Button btnBigger, btnSmaller;
	TextView tvFontDemo;
	TextView tvData;

	int fontSize = -1;

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
		ivFitable = (ImageView) innerView.findViewById(R.id.ivFitable);
		btnBigger = (Button) innerView.findViewById(R.id.btnBigger);
		btnSmaller = (Button) innerView.findViewById(R.id.btnSmaller);
		tvData = (TextView) innerView.findViewById(R.id.tvData);
		tvFontDemo = (TextView) innerView.findViewById(R.id.tvFontDemo);
	}

	@Override
	protected void initEvents() {
		btnBigger.setOnClickListener(this);
		btnSmaller.setOnClickListener(this);

	}

	@Override
	protected void initLogic() {
		ivFitable
				.setImageResource(fits[DeviceUtils.getFitable(UIUtils.getDM())]);

		fontSize = Config.cfgGetFontSize(getActivity());
		if (fontSize == -1) {
			fontSize = (int) tvFontDemo.getTextSize();
		}
		tvFontDemo.setTextSize(fontSize);
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

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnBigger:
			fontSize++;
			break;
		case R.id.btnSmaller:
			fontSize--;
			break;
		}
		tvFontDemo.setTextSize(fontSize);
		Config.cfgSetFontSize(getActivity(), fontSize);
	}

}
