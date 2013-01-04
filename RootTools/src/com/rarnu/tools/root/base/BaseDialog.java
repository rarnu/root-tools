package com.rarnu.tools.root.base;

import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.view.Window;

import com.rarnu.tools.root.fragment.GlobalFragment;

public abstract class BaseDialog extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		super.onCreate(savedInstanceState);
		
		GlobalFragment.loadFragments();

		if (getCondition()) {
			finish();
			return;
		}

		replace();
	}

	public void replace() {
		GlobalFragment.currentFragment = replaceFragment();
		getFragmentManager().beginTransaction()
				.replace(android.R.id.content, replaceFragment()).commit();
	}

	public abstract boolean getCondition();

	public abstract Fragment replaceFragment();

}
