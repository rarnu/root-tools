package com.sbbs.me.android.api;

import org.json.JSONObject;

public class SbbsMeInboxUser {

	public String UserId;
	public SbbsMeInboxUserDetail Detail;

	public static SbbsMeInboxUser fromJson(JSONObject json, String userId)
			throws Exception {
		SbbsMeInboxUser user = new SbbsMeInboxUser();
		user.UserId = userId;
		user.Detail = SbbsMeInboxUserDetail.fromJson(json);
		return user;
	}
}
