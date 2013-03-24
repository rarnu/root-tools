package com.rarnu.almanac;

import java.io.FileOutputStream;

import android.graphics.Bitmap;
import android.view.View;

public class ImageUtils {

	public static void takeScreenShot(View view, String fileName) {
		view.setDrawingCacheEnabled(true);
		view.buildDrawingCache();
		Bitmap b = view.getDrawingCache();
		savePic(b, fileName);
		view.destroyDrawingCache();
	}

	private static void savePic(Bitmap b, String fileName) {

		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(fileName);
			if (null != fos) {
				b.compress(Bitmap.CompressFormat.PNG, 100, fos);
				fos.flush();
				fos.close();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
