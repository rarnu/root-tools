package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeArticle;

public class SbbsArticleLoader extends BaseLoader<SbbsMeArticle> {

	public SbbsArticleLoader(Context context) {
		super(context);
	}

	@Override
	public List<SbbsMeArticle> loadInBackground() {
		List<SbbsMeArticle> list = null;
		try {
			list = SbbsMeAPI.getArticles();
		} catch (Exception e) {

		}
		return list;
	}

}
