package com.rarnu.tools.root.fragmentactivity;

import android.app.Fragment;

import com.rarnu.devlib.base.BaseActivity;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;


public class HtcRomActivity extends BaseActivity {

	@Override
	public Fragment replaceFragment() {
		return Fragments.getFragment(FragmentNameConst.FN_HTCROM);
	}
	
	@Override
	public int getIcon() {
		return R.drawable.icon;
	}
}
