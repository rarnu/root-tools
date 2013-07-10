package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.GithubCodeViewFragement;

public class CodeViewActivity extends BaseActivity{
	
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
		GithubCodeViewFragement blobView = 
				new GithubCodeViewFragement(repoType, sha);
		blobView.setArguments(bn);
		return blobView;
	}

}
