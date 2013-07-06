package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.GithubCodeTreeFragment;

public class CodeTreeActivity extends BaseActivity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}
	
	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		byte repoType = getIntent().getByteExtra("repoType", (byte)0);
		String sha = getIntent().getStringExtra("sha");
		bn.putByte("repoType", repoType);
		bn.putString("sha", sha);
		GithubCodeTreeFragment treef = 
				new GithubCodeTreeFragment(repoType, sha);
		treef.setArguments(bn);
		return treef;
	}

}