package com.yugioh.android.fragments;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.utils.ResourceUtils;

public class DeckFragment extends BaseFragment {

	public DeckFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_main_deck);
	}
	
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
	public int getFragmentLayoutResId() {
		return R.layout.fragment_deck;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initComponents() {

	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {

	}

	@Override
	public void initMenu(Menu arg0) {

	}

	@Override
	public void onGetNewArguments(Bundle arg0) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}
	
}
