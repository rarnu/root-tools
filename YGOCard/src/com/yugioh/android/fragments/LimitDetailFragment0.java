package com.yugioh.android.fragments;

import com.yugioh.android.R;
import com.rarnu.utils.ResourceUtils;

public class LimitDetailFragment0 extends LimitDetailFragment {

	public LimitDetailFragment0() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_main_limit_banned);
		tabTitle = ResourceUtils.getString(R.string.card_banned_pure);
		detailType = 0;
	}
}
