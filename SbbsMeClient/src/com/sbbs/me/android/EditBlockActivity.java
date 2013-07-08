package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.EditBlockFragment;

public class EditBlockActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putInt("mode", getIntent().getIntExtra("mode", -1));
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		EditBlockFragment ebf = new EditBlockFragment();
		ebf.setArguments(bn);
		return ebf;
	}

}
