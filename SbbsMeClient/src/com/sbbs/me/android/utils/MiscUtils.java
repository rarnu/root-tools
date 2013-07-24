package com.sbbs.me.android.utils;

import com.rarnu.utils.ImageUtils;

import android.graphics.drawable.Drawable;

public class MiscUtils {

	public static String extractFileNameFromURL(String url) {
		return url.substring(url.lastIndexOf("/") + 1);
	}

	public static String extractHeadUrl(String html) {
		int startPos = html.indexOf("img src=\"") + 9;
		String head = html.substring(startPos);
		head = head.substring(0, head.indexOf("\""));
		return head;
	}

	public static Drawable loadUserHeadFromFile(String headLocalName) {
		Drawable d = null;
		try {
			d = Drawable.createFromPath(headLocalName);
			d = ImageUtils.zoomDrawable(d, 256, 256);
		} catch (Exception e) {

		}
		return d;
	}
}
