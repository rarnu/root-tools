package com.sbbs.me.android.api;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;

import com.rarnu.utils.HttpRequest;

public class SbbsMeAPI {

	private static final String BASE_URL = "http://sbbs.me/";
	private static final String ARTICLE_URL = BASE_URL + "articles";

	public static List<SbbsMeArticle> getArticles() throws Exception {
		List<SbbsMeArticle> list = null;
		String ret = HttpRequest.get(ARTICLE_URL, "", HTTP.UTF_8);
		JSONArray jArr = new JSONArray(ret);
		if (jArr != null && jArr.length() != 0) {
			list = new ArrayList<SbbsMeArticle>();
			for (int i = 0; i < jArr.length(); i++) {
				list.add(SbbsMeArticle.fromJson(jArr.getJSONObject(i)));
			}
		}
		return list;
	}
}
