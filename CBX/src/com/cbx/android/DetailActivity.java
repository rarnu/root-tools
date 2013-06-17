package com.cbx.android;

import android.app.Fragment;

import com.cbx.android.fragment.DetailFragment;
import com.rarnu.devlib.base.BaseActivity;

public class DetailActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.ic_launcher;
	}

	@Override
	public Fragment replaceFragment() {
		Fragment fDetail = getFragmentManager().findFragmentByTag(
				getString(R.tag.fragment_detail));
		if (fDetail == null) {
			fDetail = new DetailFragment();
		}
		return fDetail;
	}

}
