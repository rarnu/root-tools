package com.sbbs.me.android.api;

public class SbbsMeMessage {

	/*
	 * 橙子Infinity comment to your post: comment:测试新浪微博登录. 何晓杰 now follow you.
	 * 橙子Infinity append to post a new block: 测试新浪微博登录.
	 */
	/**
	 * 0:comment, 1:follow
	 */
	public int actionType;
	public String userId;
	public String name;
	public String actionText;
	public String postId;
	public String postSubject;

	public static SbbsMeMessage fromString(String str) throws Exception {
		SbbsMeMessage message = new SbbsMeMessage();
		message.actionType = str.contains("<a href='/post/") ? 0 : 1;
		str = str.replace("<a href='/user/", "");
		message.userId = str.substring(0, str.indexOf("'"));
		str = str.substring(str.indexOf("'") + 2);
		message.name = str.substring(0, str.indexOf("<"));
		if (message.actionType == 0) {
			str = str.substring(str.indexOf("</a>") + "</a>".length());
			message.actionText = str.substring(0,
					str.indexOf("<a href='/post/"));
			str = str.substring(str.indexOf("<a href='/post/")
					+ "<a href='/post/".length());
			message.postId = str.substring(0, str.indexOf("'"));
			str = str.substring(str.indexOf("'") + 2);
			message.postSubject = str.substring(0, str.indexOf("<"));
		}
		return message;
	}
}
