package com.sbbs.me.android.dialog;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseDialog;
import com.sbbs.me.android.fragment.SelectPictureFragment;

public class SelectPictureDialog extends BaseDialog {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setFinishOnTouchOutside(true);
	}

	@Override
	public Fragment replaceFragment() {
		return new SelectPictureFragment();
	}

	@Override
	public boolean getCondition() {
		return false;
	}

}
