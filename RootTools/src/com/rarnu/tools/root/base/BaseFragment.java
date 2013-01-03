package com.rarnu.tools.root.base;


public abstract class BaseFragment extends InnerFragment {

	@Override
	protected boolean getCondition() {
		return innerView == null;
	}
}
