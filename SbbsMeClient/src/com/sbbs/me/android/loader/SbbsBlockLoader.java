package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.utils.FileUtils;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.consts.PathDefine;

public class SbbsBlockLoader extends BaseLoader<SbbsMeBlock> {

	private int page;
	private int pageSize;
	boolean refresh = false;

	public SbbsBlockLoader(Context context) {
		super(context);
	}

	public void setPage(int page, int pageSize) {
		this.page = page;
		this.pageSize = pageSize;
	}

	public void setRefresh(boolean refresh) {
		this.refresh = refresh;
	}

	public boolean isRefresh() {
		return refresh;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SbbsMeBlock> loadInBackground() {
		List<SbbsMeBlock> list = null;

		if (refresh) {
			list = SbbsMeAPI.getArticles(page, pageSize);
			if (page == 1) {
				if (list == null || list.size() == 0) {
					list = (List<SbbsMeBlock>) FileUtils
							.loadListFromFile(PathDefine.CACHE_ARTICLE_LIST_PATH);
				} else {
					FileUtils.saveListToFile(list,
							PathDefine.CACHE_ARTICLE_LIST_PATH);
				}
			}

		} else {
			list = (List<SbbsMeBlock>) FileUtils
					.loadListFromFile(PathDefine.CACHE_ARTICLE_LIST_PATH);
			if (list == null || list.size() == 0) {
				refresh = true;
				list = SbbsMeAPI.getArticles(page, pageSize);
				if (list != null && list.size() != 0) {
					if (page == 1) {
						FileUtils.saveListToFile(list,
								PathDefine.CACHE_ARTICLE_LIST_PATH);
					}
				}
			}
		}

		return list;
	}

}
