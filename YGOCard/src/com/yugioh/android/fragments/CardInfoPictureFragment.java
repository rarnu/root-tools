package com.yugioh.android.fragments;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;

public class CardInfoPictureFragment extends BaseFragment {

	CardInfo info;

	public CardInfoPictureFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	@Override
	public void onAttach(Activity activity) {
		info = (CardInfo) getActivity().getIntent().getSerializableExtra(
				"cardinfo");
		super.onAttach(activity);
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
	protected void initComponents() {

	}

	@Override
	protected void initEvents() {

	}

	@Override
	protected void initLogic() {

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_cardinfo_pic;
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

}
