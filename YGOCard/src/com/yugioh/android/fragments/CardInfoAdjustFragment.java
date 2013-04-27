package com.yugioh.android.fragments;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;

public class CardInfoAdjustFragment extends BaseFragment {

	CardInfo info;
	TextView tvAdjust;
	TextView tvNoAdjust;

	public CardInfoAdjustFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
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
	protected void initComponents() {
		tvAdjust = (TextView) innerView.findViewById(R.id.tvAdjust);
		tvNoAdjust = (TextView) innerView.findViewById(R.id.tvNoAdjust);
	}

	@Override
	protected void initEvents() {

	}

	@Override
	protected void initLogic() {
		
		info = (CardInfo) getActivity().getIntent().getSerializableExtra(
				"cardinfo");
		
		tvAdjust.setText(info.getCardAdjust());
		tvNoAdjust.setVisibility((info.getCardAdjust() == null || info
				.getCardAdjust().trim().equals("")) ? View.VISIBLE : View.GONE);
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_cardinfo_adjust;
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

}
