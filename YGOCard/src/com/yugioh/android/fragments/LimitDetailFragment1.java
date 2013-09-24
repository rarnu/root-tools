package com.yugioh.android.fragments;

import com.yugioh.android.R;
import com.yugioh.android.utils.ResourceUtils;

public class LimitDetailFragment1 extends LimitDetailFragment {

	public LimitDetailFragment1() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_main_limit_limit1);
		tabTitle = ResourceUtils.getString(R.string.card_limit1_pure);
		detailType = 1;
	}
}
