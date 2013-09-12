package com.rarnu.adcenter.utils;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.UIUtils;

public class MiscUtils {
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

	public static String getFileExtension(String path) {
		return path.substring(path.lastIndexOf(".") + 1);
	}

	public static String getFileName(String path) {
		return path.substring(path.lastIndexOf("/") + 1);
	}
}
