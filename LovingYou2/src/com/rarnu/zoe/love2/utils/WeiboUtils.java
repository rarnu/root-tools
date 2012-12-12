package com.rarnu.zoe.love2.utils;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import com.rarnu.zoe.love2.common.Config;
import com.rarnu.zoe.love2.common.GroundInfo;
import com.weibo.sdk.android.Oauth2AccessToken;
import com.weibo.sdk.android.api.StatusesAPI;
import com.weibo.sdk.android.api.WeiboAPI.FEATURE;
import com.weibo.sdk.android.net.RequestListener;

public class WeiboUtils {

	public static void shareArticleToSina(String text, String file,
			RequestListener listener) {
		StatusesAPI api = new StatusesAPI(new Oauth2AccessToken(Config.TOKEN,
				String.valueOf(Config.EXPRIED)));

		if (file == null || file.equals("")) {
			api.update(text, "0", "0", listener);
		} else {
			api.upload(text, file, "0", "0", listener);
		}
	}

	public static void getWeiboList(RequestListener listener) {
		StatusesAPI api = new StatusesAPI(new Oauth2AccessToken(Config.TOKEN,
				String.valueOf(Config.EXPRIED)));
		api.userTimeline(0, 0, 21, 1, false, FEATURE.PICTURE, false, listener);
	}

	public static List<GroundInfo> extractGroundList(String json) {
		List<GroundInfo> result = new ArrayList<GroundInfo>();

		try {
			JSONObject jo = new JSONObject(json);
			JSONArray ja = jo.getJSONArray("statuses");
			for (int i = 0; i < ja.length(); i++) {
				GroundInfo info = new GroundInfo();
				info.id = ja.getJSONObject(i).getString("id");
				info.txt = ja.getJSONObject(i).getString("text");
				info.thumb_path = ja.getJSONObject(i)
						.getString("thumbnail_pic");
				info.origin_path = ja.getJSONObject(i)
						.getString("original_pic");
				result.add(info);
			}
		} catch (Exception e) {

		}
		return result;
	}
}
