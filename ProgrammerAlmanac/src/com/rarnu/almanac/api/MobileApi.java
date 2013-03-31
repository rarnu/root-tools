package com.rarnu.almanac.api;

import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import com.rarnu.almanac.utils.HttpRequest;

import android.util.Log;

public class MobileApi {

	private static final String BASE_URL = "http://rarnu.7thgen.info/almanac/";
	public static final String DOWNLOAD_BASE_URL = BASE_URL + "download/";

	private static final String UPDATE_URL = BASE_URL + "check_update.php";
	private static final String UPDATE_PARAM = "version=%d";


	public static UpdateInfo checkUpdate(int version) {
		UpdateInfo result = null;
		try {
			String ret = HttpRequest.get(UPDATE_URL,
					String.format(UPDATE_PARAM, version), HTTP.UTF_8);
			JSONObject json = new JSONObject(ret);
			result = new UpdateInfo();
			result.result = json.getInt("result");
			result.versionCode = json.getInt("version_code");
			result.versionName = json.getString("version_name");
			result.file = json.getString("file");
			result.size = json.getString("size");
		} catch (Exception e) {
			Log.e("checkUpdate", e.getMessage());
			result = null;
		}
		return result;
	}

	
}
