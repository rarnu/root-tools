package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;

public class SbbsTagBlockListLoader extends BaseLoader<SbbsMeBlock> {

	private String tagId;
	private int page;
	private int pageSize;

	public SbbsTagBlockListLoader(Context context) {
		super(context);
	}

	public void setTagId(String tagId) {
		this.tagId = tagId;
	}

	public void setPage(int page, int pageSize) {
		this.page = page;
		this.pageSize = pageSize;
	}

	@Override
	public List<SbbsMeBlock> loadInBackground() {
		List<SbbsMeBlock> list = null;
		try {
			list = SbbsMeAPI.getArticlesViaTag(tagId, page, pageSize);
		} catch (Exception e) {

		}
		return list;
	}

}
