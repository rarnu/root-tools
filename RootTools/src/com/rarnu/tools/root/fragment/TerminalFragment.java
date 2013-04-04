package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;

public class TerminalFragment extends BaseFragment {

	@Override
	protected int getBarTitle() {
		return R.string.func_terminal;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.func_terminal_with_path;
	}

	@Override
	protected void initComponents() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void initEvents() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void initLogic() {
		// TODO Auto-generated method stub

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_terminal;
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

}
