package com.rarnu.devlib.base.inner;

import android.app.ActionBar;
import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.Window;

public abstract class InnerActivity extends Activity {

	protected ActionBar bar;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		requestWindowFeature(Window.FEATURE_ACTION_BAR);
		super.onCreate(savedInstanceState);

		if (getCondition()) {
			finish();
			return;
		}

		setContentView(getBaseLayout());

		bar = getActionBar();
		if (bar != null) {
			bar.setIcon(getIcon());
			bar.setDisplayOptions(0, ActionBar.DISPLAY_HOME_AS_UP);
			bar.setDisplayHomeAsUpEnabled(true);
		}
		replace();
	}

	public void replace() {
		getFragmentManager().beginTransaction()
				.replace(getReplaceId(), replaceFragment()).commit();
	}

	public abstract int getIcon();

	public abstract boolean getCondition();

	public abstract int getBaseLayout();

	public abstract int getReplaceId();

	public abstract Fragment replaceFragment();

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case android.R.id.home:
			finish();
			break;
		}
		return super.onOptionsItemSelected(item);
	}

}
