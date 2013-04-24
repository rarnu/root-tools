package com.yugioh.android.loader;

import android.content.Context;
import android.database.Cursor;

import com.rarnu.devlib.base.BaseCursorLoader;
import com.yugioh.android.database.YugiohUtils;

public class LimitLoader extends BaseCursorLoader {

	private int detailType;
	
	public LimitLoader(Context context, int detailType) {
		super(context);
		this.detailType = detailType;
	}

	@Override
	public Cursor loadInBackground() {
		Cursor cLimit = null;
		switch (detailType) {
		case 0:
			cLimit = YugiohUtils.getBannedCards(getContext());
			break;
		case 1:
			cLimit = YugiohUtils.getLimit1Cards(getContext());
			break;
		case 2:
			cLimit = YugiohUtils.getLimit2Cards(getContext());
			break;
		}
		return cLimit;
	}



}
