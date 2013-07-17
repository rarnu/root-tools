package com.sbbs.me.android.loader;

import android.content.Context;
import android.util.Log;

import com.rarnu.devlib.base.BaseClassLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeArticle;

public class SbbsArticleLoader extends BaseClassLoader<SbbsMeArticle> {

	private String articleId;

	public SbbsArticleLoader(Context context) {
		super(context);
	}

	public void setArticleId(String article) {
		this.articleId = article;
	}

	@Override
	public SbbsMeArticle loadInBackground() {
		SbbsMeArticle article = null;
		try {
			article = SbbsMeAPI.getArticle(articleId);
		} catch (Exception e) {
			Log.e("loadInBackground", e.getMessage());
		}
		return article;
	}

}
