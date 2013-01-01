package com.rarnu.tools.root.api;

import java.net.URLEncoder;

import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import android.util.Log;

import com.rarnu.tools.root.utils.HttpRequest;

public class MobileApi {

	// [region] const define
	private static final String BASE_URL = "http://rarnu.7thgen.info/anjuke/root_tools/";
	public static final String DOWNLOAD_BASE_URL = BASE_URL + "download/";

	private static final String UPDATE_URL = BASE_URL + "check_update.php";
	private static final String UPDATE_PARAM = "version=%d";

	private static final String FEEDBACK_URL = BASE_URL + "user_feedback.php";
	private static final String FEEDBACK_PARAM = "deviceId=%s&module=%s&os_version=%s&mail=%s&build_desc=%s&comment=%s";

	// [/region]
	
	// [region] business logic
	public static UpdateInfo checkUpdate(int version) {
		UpdateInfo result = null;
		try {
			String ret = HttpRequest.get(UPDATE_URL, String.format(UPDATE_PARAM, version), HTTP.UTF_8);
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

	public static boolean userFeedback(String deviceId, String module, String osVersion, String mail, String buildDesc,
			String comment) {
		boolean result = false;
		try {
			comment = URLEncoder.encode(comment, HTTP.UTF_8);
			String ret = HttpRequest.get(FEEDBACK_URL,
					String.format(FEEDBACK_PARAM, deviceId, module, osVersion, mail, buildDesc, comment), HTTP.UTF_8);
			JSONObject json = new JSONObject(ret);
			result = (json.getInt("result") != 0);
		} catch (Exception e) {
			result = false;
		}
		return result;
	}
	// [/region]
}
