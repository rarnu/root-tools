package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.utils.FileUtils;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeTag;
import com.sbbs.me.android.consts.PathDefine;

public class SbbsTagLoader extends BaseLoader<SbbsMeTag> {

	boolean refresh = false;

	public SbbsTagLoader(Context context) {
		super(context);
	}

	public void setRefresh(boolean refresh) {
		this.refresh = refresh;
	}
	
	public boolean isRefresh() {
		return refresh;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SbbsMeTag> loadInBackground() {
		List<SbbsMeTag> list = null;
		if (refresh) {
			list = SbbsMeAPI.getTags();
			if (list == null || list.size() == 0) {
				list = (List<SbbsMeTag>) FileUtils
						.loadListFromFile(PathDefine.CACHE_TAG_LIST_PATH);
			} else {
				FileUtils.saveListToFile(list, PathDefine.CACHE_TAG_LIST_PATH);
			}
		} else {
			list = (List<SbbsMeTag>) FileUtils
					.loadListFromFile(PathDefine.CACHE_TAG_LIST_PATH);
			if (list == null || list.size() == 0) {
				refresh = true;
				list = SbbsMeAPI.getTags();
				if (list != null && list.size() != 0) {
					FileUtils.saveListToFile(list,
							PathDefine.CACHE_TAG_LIST_PATH);
				}
			}
		}
		return list;
	}

}
