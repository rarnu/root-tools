package com.sbbs.me.android.loader;

import android.content.Context;

import com.rarnu.devlib.base.BaseClassLoader;
import com.sbbs.me.android.api.SbbsMeAPI;

public class SbbsArticleSender extends BaseClassLoader<String> {

	private String subject;
	private String tags;
	private String content;
	private String format;
	private boolean isPublic;

	public SbbsArticleSender(Context context) {
		super(context);
	}

	public void setData(String subject, String tags, String content,
			String format, boolean isPublic) {
		this.subject = subject;
		this.tags = tags;
		this.content = content;
		this.format = format;
		this.isPublic = isPublic;
	}

	@Override
	public String loadInBackground() {
		String ret = SbbsMeAPI.addNewArticle(subject, format, content,
				isPublic, tags);
		return ret;
	}

}
