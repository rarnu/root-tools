package com.rarnu.adcenter.api;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import android.util.Log;

import com.rarnu.adcenter.classes.AdItem;
import com.rarnu.adcenter.classes.QuestItem;
import com.rarnu.utils.HttpRequest;

public class AdAPI {

	private static final String HOST = "http://rarnu.7thgen.info/adcenter/api/";
	public static final String IMAGE_HOST = "http://rarnu.7thgen.info/adcenter/image/";

	public static List<AdItem> getAd(String mac, int page, int pageSize,
			int type, String extra) {
		String ret = HttpRequest.get(HOST + "get_ad.php", String.format(
				"mac=%s&page=%d&page_size=%d&type=%d&extra=%s", mac, page,
				pageSize, type, extra), HTTP.UTF_8);
		List<AdItem> list = null;
		try {
			JSONArray jarr = new JSONArray(ret);
			if (jarr != null && jarr.length() != 0) {
				list = new ArrayList<AdItem>();
				for (int i = 0; i < jarr.length(); i++) {
					list.add(AdItem.fromJson(jarr.getJSONObject(i)));
				}
			}
		} catch (Exception e) {
			Log.e("getAd", e.getMessage());
		}
		return list;
	}

	public static QuestItem getQuest(int adId) {
		String ret = HttpRequest.get(HOST + "get_quest.php",
				String.format("id=%d", adId), HTTP.UTF_8);
		QuestItem item = null;
		try {
			item = QuestItem.fromJson(new JSONObject(ret));
		} catch (Exception e) {

		}
		return item;
	}

	public static void recordClick(int adId, String mac, int userId) {
		HttpRequest.get(HOST + "record_click.php",
				String.format("id=%d&mac=%s&user_id=%d", adId, mac, userId),
				HTTP.UTF_8);
	}

	public static void recordQuest(int adId, String mac, int userId,
			boolean right) {
		HttpRequest.get(HOST + "record_quest.php", String.format(
				"id=%d&mac=%s&user_id=%d&right=%s", adId, mac, userId,
				(right ? "true" : "false")), HTTP.UTF_8);
	}
}
