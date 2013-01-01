package com.rarnu.tools.root.utils;

import java.io.File;
import java.io.FileOutputStream;

import android.graphics.Bitmap;

public class ImageUtils {
	public static void saveBitmapToFile(Bitmap bmp, String fileName) {
		try {
			File f = new File(fileName);
			f.createNewFile();
			FileOutputStream fOut = null;
			fOut = new FileOutputStream(f);
			bmp.compress(Bitmap.CompressFormat.PNG, 100, fOut);
			fOut.flush();
			fOut.close();
		} catch (Exception e) {

		}

	}
}
