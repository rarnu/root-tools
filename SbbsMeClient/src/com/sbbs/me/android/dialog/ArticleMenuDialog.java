package com.sbbs.me.android.dialog;

import android.app.Fragment;
import android.os.Bundle;
import android.view.WindowManager;

import com.rarnu.devlib.base.BaseDialog;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.fragment.ArticleMenuFragment;

public class ArticleMenuDialog extends BaseDialog {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setFinishOnTouchOutside(true);
		WindowManager.LayoutParams wmlp = getWindow().getAttributes();
		wmlp.height = UIUtils.getActionBarHeight() + UIUtils.dipToPx(298);
		getWindow().setAttributes(wmlp);
	}

	@Override
	public boolean getCondition() {
		return false;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		bn.putBoolean("isMyArticle",
				getIntent().getBooleanExtra("isMyArticle", false));
		ArticleMenuFragment amf = new ArticleMenuFragment();
		amf.setArguments(bn);
		return amf;
	}

}
