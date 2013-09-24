package com.yugioh.android.fragments;

import com.yugioh.android.R;
import com.yugioh.android.utils.ResourceUtils;

public class LimitDetailFragment2 extends LimitDetailFragment {

	public LimitDetailFragment2() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_main_limit_limit2);
		tabTitle = ResourceUtils.getString(R.string.card_limit2_pure);
		detailType = 2;
	}
}
