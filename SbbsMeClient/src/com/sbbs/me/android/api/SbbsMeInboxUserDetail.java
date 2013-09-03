package com.sbbs.me.android.api;

import org.json.JSONObject;

public class SbbsMeInboxUserDetail {

	public String AvatarURL;
	public String Created_on;
	public String Email;
	public String Id;
	public SbbsMeInboxLastMessage LastMsg;
	public String Name;
	public String Type;
	public String Updated_on;
	public String _id;
	
	public static SbbsMeInboxUserDetail fromJson(JSONObject json) throws Exception {
		SbbsMeInboxUserDetail detail = new SbbsMeInboxUserDetail();
		detail.AvatarURL = json.getString("AvatarURL");
		detail.Created_on = json.getString("Created_on");
		detail.Email = json.getString("Email");
		detail.Id = json.getString("Id");
		detail.LastMsg = SbbsMeInboxLastMessage.fromJson(json.getJSONObject("LastMsg"));
		detail.Name = json.getString("Name");
		detail.Type = json.getString("Type");
		detail.Updated_on = json.getString("Updated_on");
		detail._id = json.getString("_id");
		return detail;
	}
}
