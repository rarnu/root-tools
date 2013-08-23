package com.sbbs.me.android.utils;

import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore;

public class UriUtils {

	public static String getRealFilePath(Context context, Uri u) {
		String[] proj = { MediaStore.Images.Media.DATA };
		Cursor c = context.getContentResolver()
				.query(u, proj, null, null, null);
		String imgPath = "";
		if (c != null) {
			int index = c.getColumnIndexOrThrow(MediaStore.Images.Media.DATA);
			c.moveToFirst();
			while (!c.isAfterLast()) {
				imgPath = c.getString(index);
				c.moveToNext();
			}
			c.close();
		}
		return imgPath;
	}
}
