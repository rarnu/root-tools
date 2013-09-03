package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.utils.FileUtils;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeInboxUser;
import com.sbbs.me.android.consts.PathDefine;

public class SbbsPrivateUserLoader extends BaseLoader<SbbsMeInboxUser> {

	private boolean refresh = false;

	public SbbsPrivateUserLoader(Context context) {
		super(context);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<SbbsMeInboxUser> loadInBackground() {
		List<SbbsMeInboxUser> list = null;
		if (refresh) {
			list = SbbsMeAPI.inbox();
			if (list != null && list.size() != 0) {
				FileUtils.saveListToFile(list, PathDefine.CACHE_MSG_USER_LIST);
			} else {
				list = (List<SbbsMeInboxUser>) FileUtils.loadListFromFile(PathDefine.CACHE_MSG_USER_LIST);
			}
		} else {
			list = (List<SbbsMeInboxUser>) FileUtils.loadListFromFile(PathDefine.CACHE_MSG_USER_LIST);
			if (list == null || list.size() == 0) {
				refresh = true;
				list = SbbsMeAPI.inbox();
				if (list != null && list.size() != 0) {
					FileUtils.saveListToFile(list, PathDefine.CACHE_MSG_USER_LIST);
				}
			}
		}
		return list;
	}

	public boolean isRefresh() {
		return refresh;
	}

	public void setRefresh(boolean refresh) {
		this.refresh = refresh;
	}

}
