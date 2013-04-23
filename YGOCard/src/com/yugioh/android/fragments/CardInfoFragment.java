package com.yugioh.android.fragments;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;

import com.rarnu.devlib.base.BaseTabFragment;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.global.FragmentNames;
import com.yugioh.android.global.Fragments;

public class CardInfoFragment extends BaseTabFragment {

	CardInfo info = null;

	@Override
	protected void initLogic() {
		info = (CardInfo) getActivity().getIntent().getSerializableExtra(
				"cardinfo");
		super.initLogic();

	}

	@Override
	protected int getBarTitle() {
		return 0;
	}

	@Override
	protected int getBarTitleWithPath() {
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
	protected String getCustomTitle() {
		String title = null;
		if (info != null) {
			title = info.getSCCardName();
		}
		return title;
	}

	@Override
	public void initFragmentList(List<Fragment> listFragment) {
		listFragment.add(Fragments.getFragment(getActivity(),
				FragmentNames.FRAGMENT_CARDINFO_CARD));
		listFragment.add(Fragments.getFragment(getActivity(),
				FragmentNames.FRAGMENT_CARDINFO_ADJUST));
		listFragment.add(Fragments.getFragment(getActivity(),
				FragmentNames.FRAGMENT_CARDINFO_PICTURE));

	}

}
