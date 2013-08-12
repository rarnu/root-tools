package com.sbbs.me.android.api;

import java.io.Serializable;

import org.json.JSONObject;

public class SbbsMeUpdate implements Serializable {

	private static final long serialVersionUID = 8266362926445346684L;

	public boolean needUpdate;
	public int id;
	public int type;
	public int version;
	public String versionName;
	public String updateLog;
	public String size;
	public String publicTime;
	public String url;

	public static SbbsMeUpdate fromJson(JSONObject json) throws Exception {
		SbbsMeUpdate update = new SbbsMeUpdate();
		update.needUpdate = json.getBoolean("need_update");
		update.id = json.getInt("id");
		update.type = json.getInt("type");
		update.version = json.getInt("version");
		update.versionName = json.getString("version_name");
		update.updateLog = json.getString("update_log");
		update.size = json.getString("size");
		update.publicTime = json.getString("public_time");
		update.url = json.getString("url");
		return update;
	}
}
