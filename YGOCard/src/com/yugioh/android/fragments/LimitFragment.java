package com.yugioh.android.fragments;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseTabFragment;
import com.yugioh.android.R;

public class LimitFragment extends BaseTabFragment {

	public LimitFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}
	
	@Override
	public void initFragmentList(List<Fragment> listFragment) {
		listFragment.add(new LimitDetailFragment(getString(R.tag.tag_main_limit_banned), getString(R.string.card_banned_pure), 0));
		listFragment.add(new LimitDetailFragment(getString(R.tag.tag_main_limit_limit1), getString(R.string.card_limit1_pure), 1));
		listFragment.add(new LimitDetailFragment(getString(R.tag.tag_main_limit_limit2), getString(R.string.card_limit2_pure), 2));
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_banned;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_banned;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initMenu(Menu arg0) {

	}

	@Override
	protected void onGetNewArguments(Bundle arg0) {

	}

}
