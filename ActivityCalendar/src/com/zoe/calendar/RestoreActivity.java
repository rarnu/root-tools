package com.zoe.calendar;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.zoe.calendar.fragment.RestoreFragment;

public class RestoreActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.ic_logo;
	}

	@Override
	public Fragment replaceFragment() {
		return new RestoreFragment();
	}

    @Override
    public int customTheme() {
        return 0;
    }

}
