package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.GithubCodeViewFragement;

public class CodeViewActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putString("sha", getIntent().getStringExtra("sha"));
		bn.putString("path", getIntent().getStringExtra("path"));
		GithubCodeViewFragement blobView = new GithubCodeViewFragement(
				getIntent().getIntExtra("repoType", 0));
		blobView.setArguments(bn);
		return blobView;
	}

}
