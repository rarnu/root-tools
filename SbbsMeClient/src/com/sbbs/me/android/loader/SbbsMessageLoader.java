package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeMessage;

public class SbbsMessageLoader extends BaseLoader<SbbsMeMessage> {

	public SbbsMessageLoader(Context context) {
		super(context);
	}

	@Override
	public List<SbbsMeMessage> loadInBackground() {
		List<SbbsMeMessage> list = null;
		try {
			if (SbbsMeAPI.isLogin()) {
				list = SbbsMeAPI.getRecentMsg();
			}
		} catch (Exception e) {

		}
		return list;
	}

}
