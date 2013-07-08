package com.sbbs.me.android.dialog;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseDialog;
import com.sbbs.me.android.fragment.ArticleMenuFragment;

public class ArticleMenuDialog extends BaseDialog {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setFinishOnTouchOutside(true);
	}

	@Override
	public boolean getCondition() {
		return false;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putString("id", getIntent().getStringExtra("id"));
		bn.putBoolean("isMyArticle", getIntent().getBooleanExtra("isMyArticle", false));
		ArticleMenuFragment amf = new ArticleMenuFragment();
		amf.setArguments(bn);
		return amf;
	}

}
