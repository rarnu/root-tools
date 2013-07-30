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
		bn.putStringArrayList("image",
				getIntent().getStringArrayListExtra("image"));
		bn.putStringArrayList("url", getIntent().getStringArrayListExtra("url"));
		bn.putInt("index", getIntent().getIntExtra("index", 0));
		bn.putString("current_item", getIntent().getStringExtra("current_item"));
		BigPictureFragment bpf = new BigPictureFragment();
		bpf.setArguments(bn);
		return bpf;
	}

}
