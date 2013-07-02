package com.sbbs.me.android.api;

import java.io.Serializable;

import org.json.JSONObject;

public class SbbsMeSinaUser implements Serializable {

	private static final long serialVersionUID = -3631298488749255449L;

	public long id;
	public String screen_name;
	public String location;
	public String description;
	public String avatar_large;
	public int followers_count;
	public int friends_count;
	public int statuses_count;
	public boolean verified;
	public String verified_reason;

	public static SbbsMeSinaUser fromJson(JSONObject json) throws Exception {
		SbbsMeSinaUser user = new SbbsMeSinaUser();
		user.id = json.getLong("id");
		user.screen_name = json.getString("screen_name");
		user.location = json.getString("location");
		user.description = json.getString("description");
		user.avatar_large = json.getString("avatar_large");
		user.followers_count = json.getInt("followers_count");
		user.friends_count = json.getInt("friends_count");
		user.statuses_count = json.getInt("statuses_count");
		user.verified = json.getBoolean("verified");
		user.verified_reason = json.getString("verified_reason");
		return user;
	}

}
