package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.BlockFragment;

public class BlockActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		BlockFragment bf = new BlockFragment();
		bf.setArguments(bn);
		return bf;
	}

}
