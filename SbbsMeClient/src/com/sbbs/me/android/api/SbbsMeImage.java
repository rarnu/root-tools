package com.sbbs.me.android.api;

import java.io.Serializable;

import org.json.JSONObject;

public class SbbsMeImage implements Serializable {

	private static final long serialVersionUID = -693419073655988474L;
	public String AuthorId;
	public String Id;
	public String FileName;
	public String URL;
	public String Upload_on;
	public String _id;

	public static SbbsMeImage fromJson(JSONObject json) throws Exception {
		SbbsMeImage image = new SbbsMeImage();
		image.Id = json.getString("Id");
		image.AuthorId = json.getString("AuthorId");
		image.FileName = json.getString("FileName");
		image.URL = json.getString("URL");
		image.Upload_on = json.getString("Upload_on");
		image._id = json.getString("_id");
		return image;
	}
}
