package com.sbbs.me.android.api;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.eclipse.egit.github.core.Repository;
import org.eclipse.egit.github.core.service.RepositoryService;
import org.json.JSONArray;
import org.json.JSONObject;

import android.util.Log;

import com.rarnu.utils.HttpRequest;

public class SbbsMeAPI {

	private static final String BASE_URL = "http://sbbs.me/api/";
	private static final String ARTICLES_URL = BASE_URL + "articles";
	private static final String ARTICLE_URL = BASE_URL + "article/";

	public static List<SbbsMeBlock> getArticles() throws Exception {
		List<SbbsMeBlock> list = null;
		String ret = HttpRequest.get(ARTICLES_URL, "", HTTP.UTF_8);
		JSONArray jArr = new JSONArray(ret);
		if (jArr != null && jArr.length() != 0) {
			list = new ArrayList<SbbsMeBlock>();
			for (int i = 0; i < jArr.length(); i++) {
				list.add(SbbsMeBlock.fromJson(jArr.getJSONObject(i)));
			}
		}
		return list;
	}

	public static SbbsMeArticle getArticle(String id) throws Exception {
		SbbsMeArticle article = null;
		String ret = HttpRequest.get(ARTICLE_URL + id, "", HTTP.UTF_8);
		Log.e("getArticle", ret);
		article = SbbsMeArticle.fromJson(new JSONObject(ret));
		return article;
	}
	
	public static List<Repository> getRepos() {
		List<Repository> list = null;
		RepositoryService service = new RepositoryService();
		Repository mobileRepo = new Repository();
		Repository webRepo = new Repository();
		try {
			mobileRepo = service.getRepository("rarnu", "root-tools");
			webRepo = service.getRepository("zhuangbiaowei", "sbbsme");
		} catch (Exception e) {
		}
		Log.e("getMobileRepo", mobileRepo.getName());
		Log.e("getWebRepo", webRepo.getName());
		list = new ArrayList<Repository>();
		list.add(mobileRepo);
		list.add(webRepo);
		return list;
	}
}
