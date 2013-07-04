package com.sbbs.me.android.api;

import java.io.Serializable;

import org.json.JSONObject;

public class SbbsMeGoogleUser implements Serializable {

	private static final long serialVersionUID = -3631298488749255449L;

	public String id;
	public String email;
	public boolean verified_email;
	public String name;
	public String given_name;
	public String family_name;
	public String link;
	public String picture;
	public String gender;
	public String birthday;
	public String locale;

	public static SbbsMeGoogleUser fromJson(JSONObject json) throws Exception {
		SbbsMeGoogleUser user = new SbbsMeGoogleUser();
		user.id = json.getString("id");
		user.email = json.getString("email");
		user.verified_email = json.getBoolean("verified_email");
		user.name = json.getString("name");
		user.given_name = json.getString("given_name");
		user.family_name = json.getString("family_name");
		user.link = json.getString("link");
		user.picture = json.getString("picture");
		user.gender = json.getString("gender");
		user.birthday = json.getString("birthday");
		user.locale = json.getString("locale");
		return user;
	}

}
