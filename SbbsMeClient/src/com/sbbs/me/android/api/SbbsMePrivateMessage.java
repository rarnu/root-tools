package com.sbbs.me.android.api;

import org.json.JSONObject;

public class SbbsMePrivateMessage {

	public String _id;
	public String Id;
	public String FromUserId;
	public String FromUserName;
	public String ToUserId;
	public String ToUserName;
	public String Format;
	public String Body;
	public String Created_on;

	public boolean read = false;
	
	public static SbbsMePrivateMessage fromJson(JSONObject json)
			throws Exception {
		SbbsMePrivateMessage msg = new SbbsMePrivateMessage();
		msg._id = json.getString("_id");
		msg.Id = json.getString("Id");
		msg.FromUserId = json.getString("FromUserId");
		msg.FromUserName = json.getString("FromUserName");
		msg.ToUserId = json.getString("ToUserId");
		msg.ToUserName = json.getString("ToUserName");
		msg.Format = json.getString("Format");
		msg.Body = json.getString("Body");
		msg.Created_on = json.getString("Created_on");
		return msg;
	}
}
