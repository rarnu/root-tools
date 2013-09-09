package com.rarnu.adcenter;

import android.app.Fragment;
import android.os.Bundle;
import android.view.WindowManager;

import com.rarnu.adcenter.fragment.AnswerFragment;
import com.rarnu.devlib.base.BaseDialog;
import com.rarnu.utils.UIUtils;

public class AnswerActivity extends BaseDialog {

	AnswerFragment aFragment;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		aFragment = new AnswerFragment();
		super.onCreate(savedInstanceState);
		WindowManager.LayoutParams wlp = getWindow().getAttributes();
		wlp.x = -(UIUtils.getWidth() / 2);
		wlp.y = UIUtils.getHeight() / 2;
		getWindow().setAttributes(wlp);
	}

	@Override
	public boolean getCondition() {
		return false;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		aFragment.setArguments(bn);
		return aFragment;
	}

}
