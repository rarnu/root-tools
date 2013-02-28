package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.fragment.GlobalFragment;

public class HostMainActivity extends BaseActivity {

	@Override
	public Fragment replaceFragment() {
		return GlobalFragment.fHost;
	}
	
	@Override
	public int getIcon() {
		return R.drawable.icon;
	}

}
