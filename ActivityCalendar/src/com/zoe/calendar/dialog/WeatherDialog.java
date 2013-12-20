package com.zoe.calendar.dialog;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseDialog;
import com.zoe.calendar.R;
import com.zoe.calendar.fragment.WeatherDialogFragment;

public class WeatherDialog extends BaseDialog {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setFinishOnTouchOutside(false);
		setTitle(R.string.weather_hint);
	}

	@Override
	public boolean getCondition() {
		return false;
	}

	@Override
	public Fragment replaceFragment() {
		return new WeatherDialogFragment();
	}

    @Override
    public int customTheme() {
        return 0;
    }

}
