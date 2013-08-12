package com.sbbs.me.android.utils;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.UIUtils;

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
			d = resizeActionIcon(d);
		} catch (Exception e) {

		}
		return d;
	}

	public static Drawable loadResIcon(Context context, int res) {
		Drawable d = null;
		try {
			Bitmap bmp = BitmapFactory.decodeResource(context.getResources(),
					res);
			d = new BitmapDrawable(bmp);
			d = resizeActionIcon(d);
		} catch (Exception e) {

		}
		return d;
	}
	
	private static Drawable resizeActionIcon(Drawable drawable) {
		int heightBase = UIUtils.dipToPx(24);
		int height = (int) (heightBase * UIUtils.getDensity());
		Drawable d = ImageUtils.zoomDrawable(drawable, height, height);
		return d;
	}
}
