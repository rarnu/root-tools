package com.rarnu.adcenter;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.adcenter.fragment.AdDetailFragment;
import com.rarnu.devlib.base.BaseActivity;

public class AdDetailActivity extends BaseActivity {

	AdDetailFragment adFragment;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		adFragment = new AdDetailFragment();
		super.onCreate(savedInstanceState);
	}

	@Override
	public int getIcon() {
		return R.drawable.ic_launcher;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		adFragment.setArguments(bn);
		return adFragment;
	}

}
