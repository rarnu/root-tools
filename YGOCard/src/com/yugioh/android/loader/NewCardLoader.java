package com.yugioh.android.loader;

import android.content.Context;
import android.database.Cursor;

import com.rarnu.devlib.base.BaseCursorLoader;
import com.yugioh.android.database.YugiohUtils;

public class NewCardLoader extends BaseCursorLoader {

	public NewCardLoader(Context context) {
		super(context);
	}

	@Override
	public Cursor loadInBackground() {
		return YugiohUtils.getLatest100(getContext());
	}

}
