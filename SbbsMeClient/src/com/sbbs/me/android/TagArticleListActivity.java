package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.TagArticleListFragment;

public class TagArticleListActivity extends BaseActivity {

	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		TagArticleListFragment talf = new TagArticleListFragment();
		talf.setArguments(bn);
		return talf;
	}

}
