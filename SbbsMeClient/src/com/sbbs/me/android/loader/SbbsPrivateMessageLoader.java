package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMePrivateMessage;
import com.sbbs.me.android.database.PrivateMessageUtils;

public class SbbsPrivateMessageLoader extends BaseLoader<SbbsMePrivateMessage> {

	private String userId;
	private int page;
	private int pageSize;
	private boolean refresh = false;

	public SbbsPrivateMessageLoader(Context context) {
		super(context);
	}

	public void setQuery(String userId, int page, int pageSize) {
		this.userId = userId;
		this.page = page;
		this.pageSize = pageSize;
	}

	@Override
	public List<SbbsMePrivateMessage> loadInBackground() {

		List<SbbsMePrivateMessage> list = null;
		if (refresh) {
			list = SbbsMeAPI.getPrivateMessage(userId, page, pageSize);
			if (list != null && list.size() != 0) {
				PrivateMessageUtils.saveMessages(getContext(), list);
			} else {
				if (page == 1) {
					list = PrivateMessageUtils.queryMessages(getContext(), userId);
				}
			}
		} else {
			list = PrivateMessageUtils.queryMessages(getContext(), userId);
			if (list == null || list.size() == 0) {
				refresh = true;
				list = SbbsMeAPI.getPrivateMessage(userId, page, pageSize);
				if (list != null && list.size() != 0) {
					PrivateMessageUtils.saveMessages(getContext(), list);
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
