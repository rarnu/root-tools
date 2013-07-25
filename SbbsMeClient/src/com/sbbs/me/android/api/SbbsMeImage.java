package com.sbbs.me.android.api;

import java.io.Serializable;

import org.json.JSONObject;

public class SbbsMeImage implements Serializable {

	private static final long serialVersionUID = -693419073655988474L;
	public String AuthorId;
	public String Id;
	public String Desc;
	public String Src;
	public String Created_on;
	public String _id;

	public static SbbsMeImage fromJson(JSONObject json) throws Exception {
		SbbsMeImage image = new SbbsMeImage();
		image.Id = json.getString("Id");
		image.AuthorId = json.getString("AuthorId");
		image.Desc = json.getString("Desc");
		image.Src = json.getString("Src");
		image.Created_on = json.getString("Created_on");
		image._id = json.getString("_id");
		return image;
	}
}
