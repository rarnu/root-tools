package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.ViewMessageFragment;

public class ViewMessageActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		ViewMessageFragment vmf = new ViewMessageFragment();
		Bundle bn = new Bundle();
		bn.putString("id", getIntent().getStringExtra("id"));
		bn.putString("name", getIntent().getStringExtra("name"));
		bn.putString("avatar", getIntent().getStringExtra("avatar"));
		vmf.setArguments(bn);
		return vmf;
	}

}
