package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.BigPictureFragment;

public class BigPictureActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putString("image", getIntent().getStringExtra("image"));
		BigPictureFragment bpf = new BigPictureFragment();
		bpf.setArguments(bn);
		return bpf;
	}

}
