package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMePrivateMessage;
import com.sbbs.me.android.api.SbbsMeUserLite;
import com.sbbs.me.android.database.PrivateMessageUtils;

public class SbbsPrivateMessageLoader extends BaseLoader<SbbsMeUserLite> {

	private String lastMsgId;
	private int page;
	private int pageSize;
	private boolean refresh = false;

	public SbbsPrivateMessageLoader(Context context) {
		super(context);
	}

	public void setQuery(String lastMsgId, int page, int pageSize) {
		this.lastMsgId = lastMsgId;
		this.page = page;
		this.pageSize = pageSize;
	}

	@Override
	public List<SbbsMeUserLite> loadInBackground() {
		List<SbbsMePrivateMessage> list = null;
		if (refresh) {
			list = SbbsMeAPI.queryMsg(lastMsgId, page, pageSize);
			if (list != null && list.size() != 0) {
				PrivateMessageUtils.saveMessages(getContext(), list);
			}
		} else {
			list = PrivateMessageUtils.queryMessages(getContext());
			if (list == null || list.size() == 0) {
				list = SbbsMeAPI.queryMsg(lastMsgId, page, pageSize);
				if (list != null && list.size() != 0) {
					PrivateMessageUtils.saveMessages(getContext(), list);
				}
			}
		}
		List<SbbsMeUserLite> listUser = null;
		if (list != null) {
			listUser = PrivateMessageUtils.getMessageUsers(list);
		}
		return listUser;
	}

	public boolean isRefresh() {
		return refresh;
	}

	public void setRefresh(boolean refresh) {
		this.refresh = refresh;
	}

}
