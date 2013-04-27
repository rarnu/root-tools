package com.yugioh.android.fragments;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;

public class DeckFragment extends BaseFragment {

	public DeckFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_deck;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_deck;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_deck;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initComponents() {

	}

	@Override
	protected void initEvents() {

	}

	@Override
	protected void initLogic() {

	}

	@Override
	protected void initMenu(Menu arg0) {

	}

	@Override
	protected void onGetNewArguments(Bundle arg0) {

	}

}
