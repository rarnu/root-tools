package com.sbbs.me.android.api;

import org.json.JSONObject;

public class SbbsMeVideo extends SbbsMeImage {

	private static final long serialVersionUID = -3074362795514143087L;
	public boolean VideoType = true;
	public String ThumbUrl = "";

	public static SbbsMeVideo fromJson(JSONObject json) throws Exception {
		SbbsMeVideo video = new SbbsMeVideo();
		video.Id = json.getString("Id");
		video.AuthorId = json.getString("AuthorId");
		video.FileName = json.getString("FileName");
		video.URL = json.getString("URL");
		video.Upload_on = json.getString("Upload_on");
		video._id = json.getString("_id");
		video.VideoType = json.getBoolean("VideoType");
		video.ThumbUrl = json.getString("ThumbUrl");
		return video;
	}
}
