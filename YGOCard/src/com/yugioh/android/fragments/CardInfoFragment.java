package com.yugioh.android.fragments;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;

import com.rarnu.devlib.base.BaseTabFragment;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.common.MenuIds;

public class CardInfoFragment extends BaseTabFragment {

	MenuItem itemShare;
	
	public CardInfoFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	CardInfo info = null;

	@Override
	protected void initLogic() {
		info = (CardInfo) getActivity().getIntent().getSerializableExtra(
				"cardinfo");
		super.initLogic();

	}

	@Override
	public int getBarTitle() {
		return 0;
	}

	@Override
	public int getBarTitleWithPath() {
		return 0;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initMenu(Menu menu) {
		itemShare = menu.add(0, MenuIds.MENUID_SHARE, 99, R.string.share);
		itemShare.setIcon(android.R.drawable.ic_menu_share);
		itemShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public String getCustomTitle() {
		String title = null;
		if (info != null) {
			title = info.getSCCardName();
		}
		return title;
	}

	@Override
	public void initFragmentList(List<Fragment> listFragment) {

		listFragment.add(new CardInfoCardFragment(
				getString(R.tag.tag_card_info),
				getString(R.string.page_cardinfo)));
		listFragment.add(new CardInfoAdjustFragment(
				getString(R.tag.tag_card_adjust),
				getString(R.string.page_cardadjust)));
		listFragment
				.add(new CardInfoPictureFragment(getString(R.tag.tag_card_pic),
						getString(R.string.page_picture)));

	}

}
