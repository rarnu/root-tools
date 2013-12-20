package com.zoe.calendar;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.zoe.calendar.fragment.CityFragment;

public class CityActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.ic_logo;
	}

	@Override
	public Fragment replaceFragment() {
		return new CityFragment();
	}

    @Override
    public int customTheme() {
        return 0;
    }

}
