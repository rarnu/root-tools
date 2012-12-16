package com.rarnu.zoe.love2.api;

import java.net.URLEncoder;

import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import android.content.Context;
import android.util.Log;

import com.rarnu.zoe.love2.common.Config;
import com.rarnu.zoe.love2.utils.HttpRequest;
import com.rarnu.zoe.love2.utils.MiscUtils;

public class LovingYouApi {

	private static final String API_HOST = "http://rarnu.7thgen.info/lovingyou/";
	private static final String GET_TOKEN = API_HOST + "get_token.php";
	private static final String UPDATE_TOKEN = API_HOST + "update_token.php";
	private static final String SAVE_LOG = API_HOST + "save_log.php";

	public static void getToken(Context context) {
		Config.TOKEN = Config.getSinaToken(context);
		Config.EXPRIED = Config.getSinaExpired(context);
		String result = HttpRequest.get(GET_TOKEN, "", HTTP.UTF_8);
		try {
			JSONObject json = new JSONObject(result);
			String t = json.getString("token");
			String l = json.getString("time");
			if (l.equals("")) {
				l = "0";
			}
			if (!t.equals("") && !l.equals("0")) {
				if (!Config.TOKEN.equals(t)) {
					Config.TOKEN = t;
					Config.setSinaToken(context, Config.TOKEN);
				}
				if (!(Config.EXPRIED == Long.parseLong(l))) {
					Config.EXPRIED = Long.parseLong(l);
					Config.setSinaExpired(context, Config.EXPRIED);
				}
			}
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

	public static void saveLog(final Context context, final String page,
			final String click) {

		new Thread(new Runnable() {
			@Override
			public void run() {

				String uClick = click;
				String uModule = MiscUtils.getDeviceModule();
				try {
					uClick = URLEncoder.encode(uClick, HTTP.UTF_8);
					uModule = URLEncoder.encode(uModule, HTTP.UTF_8);
				} catch (Exception e) {

				}

				HttpRequest.get(SAVE_LOG, String.format(
						"device=%s&page=%s&click=%s&module=%s",
						MiscUtils.getDeviceUniqueId(context), page, uClick,
						uModule), HTTP.UTF_8);

			}
		}).start();

	}
}
