package com.sbbs.me.android.dialog;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseDialog;
import com.sbbs.me.android.fragment.UpdateInfoFragment;

public class UpdateInfoDialog extends BaseDialog {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setFinishOnTouchOutside(false);
	}
	
	@Override
	public boolean getCondition() {
		return false;
	}

	@Override
	public Fragment replaceFragment() {
		UpdateInfoFragment uif = new UpdateInfoFragment();
		Bundle bn = new Bundle();
		bn.putSerializable("update", getIntent().getSerializableExtra("update"));
		uif.setArguments(bn);
		return uif;
	}

}
