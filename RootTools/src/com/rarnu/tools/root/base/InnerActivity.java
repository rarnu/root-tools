package com.rarnu.tools.root.base;

import android.app.ActionBar;
import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.Window;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.fragment.GlobalFragment;

public abstract class InnerActivity extends Activity {

	protected ActionBar bar;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_ACTION_BAR);
		
		GlobalFragment.loadFragments();

		if (getCondition()) {
			finish();
			return;
		}

		setContentView(getBaseLayout());

		bar = getActionBar();
		bar.setIcon(R.drawable.icon);
		bar.setDisplayOptions(0, ActionBar.DISPLAY_HOME_AS_UP);
		bar.setDisplayHomeAsUpEnabled(true);

		redo();
	}

	@Override
	protected void onResume() {
		GlobalFragment.loadFragments();
		super.onResume();
		redo();
	}

	public void redo() {
		getFragmentManager().beginTransaction()
				.replace(getReplaceId(), replaceFragment()).commit();
	}

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
