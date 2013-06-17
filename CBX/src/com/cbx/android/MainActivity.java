package com.cbx.android;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;

import com.cbx.android.fragment.DetailFragment;
import com.cbx.android.fragment.IndexFragment;
import com.cbx.android.utils.ResourceUtils;
import com.rarnu.devlib.base.BaseMainActivity;
import com.rarnu.utils.UIUtils;

public class MainActivity extends BaseMainActivity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), true);
		ResourceUtils.init(this);

		super.onCreate(savedInstanceState);
	}

	@Override
	public void loadFragments() {

	}

	@Override
	public void releaseFragments() {

	}

	@Override
	public void initOnce() {

	}

	@Override
	public String getBarTitle() {
		return getString(R.string.app_name);
	}

	@Override
	public Fragment getFragment(int currentFragment) {
		Fragment fDetail = getFragmentManager().findFragmentByTag(
				getString(R.tag.fragment_detail));
		if (fDetail == null) {
			fDetail = new DetailFragment();
		}
		return fDetail;
	}

	@Override
	public Fragment getIndexFragment() {
		return new IndexFragment();
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onHomeClick() {

	}

	@Override
	public void onRecentAppClick() {

	}

}
