package com.sbbs.me.android.api;

public class SbbsMeArticleObject {

	/**
	 * 0: url, 1:image
	 */
	public int objType;
	public String text;
	public String url;

	public SbbsMeArticleObject(int objType, String text, String url) {
		this.objType = objType;
		this.text = text;
		this.url = url;
	}
}
