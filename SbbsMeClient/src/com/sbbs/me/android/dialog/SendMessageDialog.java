package com.sbbs.me.android.dialog;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseDialog;
import com.sbbs.me.android.fragment.SendMessageFragment;

public class SendMessageDialog extends BaseDialog {

	@Override
	public boolean getCondition() {
		return false;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putString("user", getIntent().getStringExtra("user"));
		SendMessageFragment smf = new SendMessageFragment();
		smf.setArguments(bn);
		return smf;
	}
}
