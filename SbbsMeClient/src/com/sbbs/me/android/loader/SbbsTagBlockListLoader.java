package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;

public class SbbsTagBlockListLoader extends BaseLoader<SbbsMeBlock> {

	private String tagId;
	
	public SbbsTagBlockListLoader(Context context) {
		super(context);
	}
	
	public void setTagId(String tagId) {
		this.tagId = tagId;
	}

	@Override
	public List<SbbsMeBlock> loadInBackground() {
		List<SbbsMeBlock> list = null;
		try {
			list = SbbsMeAPI.getArticlesViaTag(tagId);
		} catch (Exception e) {

		}
		return list;
	}

}
