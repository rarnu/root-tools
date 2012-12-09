package com.rarnu.zoe.love2.api;

import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import android.util.Log;

import com.rarnu.zoe.love2.common.Config;
import com.rarnu.zoe.love2.utils.HttpRequest;

public class LovingYouApi {

	private static final String API_HOST = "http://rarnu.7thgen.info/lovingyou/";
	private static final String GET_TOKEN = API_HOST + "get_token.php";
	private static final String UPDATE_TOKEN = API_HOST + "update_token.php";

	public static void getToken() {
		String result = HttpRequest.get(GET_TOKEN, "", HTTP.UTF_8);
		try {
			JSONObject json = new JSONObject(result);
			Config.TOKEN = json.getString("token");
			Config.EXPRIED = json.getString("time");
		} catch (Exception e) {
			Log.e("getToken error", e.getMessage());
		}
	}

	public static boolean updateToken(String token, String time) {
		String result = HttpRequest.get(UPDATE_TOKEN,
				String.format("token=%s&time=%s", token, time), HTTP.UTF_8);
		boolean ret = false;
		try {
			JSONObject json = new JSONObject(result);
			ret = (json.getInt("result") == 1);
		} catch (Exception e) {
			Log.e("updateToken error", e.getMessage());
			ret = false;
		}
		return ret;
	}
}
