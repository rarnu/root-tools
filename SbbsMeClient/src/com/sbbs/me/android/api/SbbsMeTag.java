package com.sbbs.me.android.api;

import org.json.JSONObject;

public class SbbsMeTag {

	public String Description;
	public String Id;
	public String Name;
	public String _id;

	public static SbbsMeTag fromJson(JSONObject json) throws Exception {
		SbbsMeTag tag = new SbbsMeTag();
		tag.Description = json.getString("Description");
		tag.Id = json.getString("Id");
		tag.Name = json.getString("Name");
		tag._id = json.getString("_id");
		return tag;
	}
}
