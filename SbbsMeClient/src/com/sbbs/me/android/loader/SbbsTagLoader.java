package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeTag;

public class SbbsTagLoader extends BaseLoader<SbbsMeTag> {

	public SbbsTagLoader(Context context) {
		super(context);
	}

	@Override
	public List<SbbsMeTag> loadInBackground() {
		List<SbbsMeTag> list = null;
		try {
			list = SbbsMeAPI.getTags();
		} catch (Exception e) {

		}
		return list;
	}

}
