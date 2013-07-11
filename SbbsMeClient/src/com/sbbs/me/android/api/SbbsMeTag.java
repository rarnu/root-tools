package com.sbbs.me.android.api;

import java.io.Serializable;

import org.json.JSONObject;

public class SbbsMeTag implements Serializable {
	private static final long serialVersionUID = -7850068286008267099L;
	public String Description;
	public String Id;
	public String Name;
	public String _id;
	public int BlockCount;

	public static SbbsMeTag fromJson(JSONObject json) throws Exception {
		SbbsMeTag tag = new SbbsMeTag();
		tag.Description = json.getString("Description");
		tag.Id = json.getString("Id");
		tag.Name = json.getString("Name");
		tag._id = json.getString("_id");
		tag.BlockCount = json.getInt("BlockCount");
		return tag;
	}
}
