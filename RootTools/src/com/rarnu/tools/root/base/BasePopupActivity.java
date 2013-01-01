package com.rarnu.tools.root.base;

import com.rarnu.tools.root.R;

public abstract class BasePopupActivity extends InnerActivity {

	@Override
	public boolean getCondition() {
		return false;
	}

	@Override
	public int getBaseLayout() {
		return R.layout.layout_popup_replacement;
	}

	@Override
	public int getReplaceId() {
		return R.id.fPopupReplacement;
	}

}
