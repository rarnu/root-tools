package com.sbbs.me.android.utils;

import java.io.File;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.consts.PathDefine;

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
	
	public static Drawable getUserHead(Context context, String url, String local) {
		String headLocalPath = PathDefine.ROOT_PATH;
		if (!new File(headLocalPath).exists()) {
			new File(headLocalPath).mkdirs();
		}
		String headLocalName = headLocalPath + local;
		Config.setHeadPath(context, headLocalName);
		if (!new File(headLocalName).exists()) {
			DownloadUtils.downloadFile(url, headLocalName, null);
		}
		return loadUserHeadFromFile(headLocalName);
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
