package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.UserDetailFragment;

public class UserDetailActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putString("user", getIntent().getStringExtra("user"));
		UserDetailFragment udf = new UserDetailFragment();
		udf.setArguments(bn);
		return udf;
	}

    @Override
    public int customTheme() {
        return 0;
    }

}
