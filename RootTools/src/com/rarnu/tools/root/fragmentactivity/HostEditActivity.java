package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;

import com.rarnu.devlib.base.BasePopupActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.fragment.GlobalFragment;

public class HostEditActivity extends BasePopupActivity {	

	@Override
	public Fragment replaceFragment() {
		return GlobalFragment.fHostEdit;
	}
	
	@Override
	public int getIcon() {
		return R.drawable.icon;
	}

}
