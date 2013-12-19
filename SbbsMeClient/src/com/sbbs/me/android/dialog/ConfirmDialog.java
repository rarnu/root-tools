package com.sbbs.me.android.dialog;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseDialog;
import com.sbbs.me.android.fragment.ConfirmFragment;

public class ConfirmDialog extends BaseDialog {

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
		bn.putBoolean("ok", getIntent().getBooleanExtra("ok", true));
		bn.putBoolean("cancel", getIntent().getBooleanExtra("cancel", true));
		bn.putString("text", getIntent().getStringExtra("text"));
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		ConfirmFragment cf = new ConfirmFragment();
		cf.setArguments(bn);
		return cf;
	}

    @Override
    public int customTheme() {
        return 0;
    }

}
