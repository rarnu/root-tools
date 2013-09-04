package com.sbbs.me.android.api;

import org.json.JSONObject;

public class SbbsMeInboxLastMessage {

	public String Body;
	public String Created_on;
	public String FromUserId;
	public String FromUserName;
	public String Id;
	public String ToUserId;
	public String ToUserName;
	public String _id;

	public static SbbsMeInboxLastMessage fromJson(JSONObject json)
			throws Exception {
		SbbsMeInboxLastMessage msg = new SbbsMeInboxLastMessage();
		msg.Body = json.getString("Body");
		msg.Created_on = json.getString("Created_on");
		msg.FromUserId = json.getString("FromUserId");
		msg.FromUserName = json.getString("FromUserName");
		msg.Id = json.getString("Id");
		msg.ToUserId = json.getString("ToUserId");
		msg.ToUserName = json.getString("ToUserName");
		msg._id = json.getString("_id");
		return msg;
	}
}
