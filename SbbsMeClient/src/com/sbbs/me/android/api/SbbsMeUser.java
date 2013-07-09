package com.sbbs.me.android.api;

import java.io.Serializable;

import org.json.JSONObject;

public class SbbsMeUser implements Serializable {

	private static final long serialVersionUID = 6707997513175038028L;
	public String AvatarURL;
	public String Created_on;
	public String Email;
	public String Id;
	public String Name;
	public String Type;
	public String Updated_on;
	public String _id;

	public static SbbsMeUser fromJson(JSONObject json) throws Exception {
		SbbsMeUser user = new SbbsMeUser();
		user.AvatarURL = json.getString("AvatarURL");
		user.Created_on = json.getString("Created_on");
		user.Email = json.getString("Email");
		user.Id = json.getString("Id");
		user.Name = json.getString("Name");
		user.Type = json.getString("Type");
		user.Updated_on = json.getString("Updated_on");
		user._id = json.getString("_id");
		return user;
	}

}
