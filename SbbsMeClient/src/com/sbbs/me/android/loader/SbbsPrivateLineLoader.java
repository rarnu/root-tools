package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMePrivateMessage;
import com.sbbs.me.android.database.PrivateMessageUtils;

public class SbbsPrivateLineLoader extends BaseLoader<SbbsMePrivateMessage> {

	private String toUserId;

	public SbbsPrivateLineLoader(Context context) {
		super(context);
	}

	public void setUserId(String toUserId) {
		this.toUserId = toUserId;
	}

	@Override
	public List<SbbsMePrivateMessage> loadInBackground() {
		List<SbbsMePrivateMessage> list = PrivateMessageUtils.queryMessages(
				getContext(), toUserId);
		return list;
	}

}
