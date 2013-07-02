package com.sbbs.me.android.utils;


public class MiscUtils {

	public static String extractHeadUrl(String html) {
		int startPos = html.indexOf("img src=\"") + 9;
		String head = html.substring(startPos);
		head = head.substring(0, head.indexOf("\""));
		return head;
	}
}
