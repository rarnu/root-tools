package com.sbbs.me.android.loader;

import android.content.Context;

import com.rarnu.devlib.base.BaseClassLoader;
import com.sbbs.me.android.api.SbbsMeAPI;

public class SbbsPrivateMessageSender extends BaseClassLoader<String> {

	private String toUserId;
	private String format;
	private String body;

	public SbbsPrivateMessageSender(Context context) {
		super(context);
	}

	public void setMessage(String toUserId, String format, String body) {
		this.toUserId = toUserId;
		this.format = format;
		this.body = body;
	}

	@Override
	public String loadInBackground() {
		String ret = SbbsMeAPI.sendMsg(toUserId, format, body);
		return ret;
	}

}
