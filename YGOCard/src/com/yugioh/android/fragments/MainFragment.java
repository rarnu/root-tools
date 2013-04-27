package com.yugioh.android.fragments;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseTabFragment;
import com.yugioh.android.R;
import com.yugioh.android.common.MenuIds;

public class MainFragment extends BaseTabFragment {

	public MainFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	MenuItem itemSearch;
	MenuItem itemReset;

	@Override
	public int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initMenu(Menu menu) {
		itemSearch = menu.add(0, MenuIds.MENUID_SEARCH, 98,
				R.string.search_search);
		itemSearch.setIcon(android.R.drawable.ic_menu_search);
		itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		itemReset = menu
				.add(0, MenuIds.MENUID_RESET, 99, R.string.search_reset);
		itemReset.setIcon(android.R.drawable.ic_menu_close_clear_cancel);
		itemReset.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		Bundle bn = new Bundle();

		BaseFragment fSearch = (BaseFragment) getFragmentManager()
				.findFragmentByTag(getString(R.tag.tag_main_search));

		switch (item.getItemId()) {
		case MenuIds.MENUID_SEARCH:
			if (getCurrentPage() == 0) {
				bn.putString("data", "search");
				fSearch.setNewArguments(bn);
			} else {
				setTabPosition(0);
			}
			break;
		case MenuIds.MENUID_RESET:
			bn.putString("data", "reset");
			fSearch.setNewArguments(bn);
			if (getCurrentPage() != 0) {
				setTabPosition(0);
			}
			break;
		}
		return true;
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initFragmentList(List<Fragment> listFragment) {
		listFragment.add(new SearchFragment(getString(R.tag.tag_main_search),
				getString(R.string.page_search)));
		listFragment
				.add(new SearchResultFragment(getString(R.tag.tag_main_result),
						getString(R.string.page_list)));
	}

}
