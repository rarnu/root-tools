package com.yugioh.android.fragments;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseTabFragment;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;

public class CardInfoFragment extends BaseTabFragment {

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
