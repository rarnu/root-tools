package com.sbbs.me.android.api;

import org.json.JSONObject;

public class SbbsMeWeibo {

	public String name;
	public String desc;
	public String github;
	public String weibo;

	public static SbbsMeWeibo fromJson(JSONObject json) throws Exception {
		SbbsMeWeibo weibo = new SbbsMeWeibo();
		weibo.name = json.getString("name");
		weibo.desc = json.getString("desc");
		weibo.weibo = json.getString("weibo");
		weibo.github = json.getString("github");
		return weibo;
	}
}
