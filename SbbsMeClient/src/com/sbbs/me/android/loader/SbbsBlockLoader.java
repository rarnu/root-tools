package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;

public class SbbsBlockLoader extends BaseLoader<SbbsMeBlock> {

	private int page;
	private int pageSize;
	
	public SbbsBlockLoader(Context context) {
		super(context);
	}
	
	public void setPage(int page, int pageSize) {
		this.page = page;
		this.pageSize = pageSize;
	}

	@Override
	public List<SbbsMeBlock> loadInBackground() {
		List<SbbsMeBlock> list = null;
		try {
			list = SbbsMeAPI.getArticles(page, pageSize);
		} catch (Exception e) {

		}
		return list;
	}

}
