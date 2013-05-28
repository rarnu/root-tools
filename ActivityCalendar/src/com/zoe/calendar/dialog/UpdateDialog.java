package com.zoe.calendar.dialog;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseDialog;
import com.zoe.calendar.R;
import com.zoe.calendar.fragment.UpdateDialogFragment;

public class UpdateDialog extends BaseDialog {

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
		return new UpdateDialogFragment(getString(R.tag.fragment_update_dialog));
	}

}
