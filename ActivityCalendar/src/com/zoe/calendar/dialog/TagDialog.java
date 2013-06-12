package com.zoe.calendar.dialog;

import android.app.Fragment;
import android.os.Bundle;

import com.rarnu.devlib.base.BaseDialog;
import com.zoe.calendar.R;
import com.zoe.calendar.fragment.TagDialogFrament;

public class TagDialog extends BaseDialog {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setFinishOnTouchOutside(false);
		setTitle(R.string.tag_title);
	}

	@Override
	public boolean getCondition() {
		return false;
	}

	@Override
	public Fragment replaceFragment() {
		return new TagDialogFrament();
	}

}
