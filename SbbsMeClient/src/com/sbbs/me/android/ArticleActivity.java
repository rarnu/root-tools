package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseActivity;
import com.sbbs.me.android.fragment.ArticleFragment;

public class ArticleActivity extends BaseActivity {
	
	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putString("articleId", getIntent().getStringExtra("articleId"));
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		ArticleFragment af = new ArticleFragment();
		af.setArguments(bn);
		return af;
	}

}
