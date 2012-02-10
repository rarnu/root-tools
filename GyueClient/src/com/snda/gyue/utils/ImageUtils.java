package com.snda.gyue.utils;

import java.io.File;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

public class ImageUtils {

	public static Drawable loadItemImage(Context context, String path) {
		return loadImage(context, path, 2, 80, 50);
	}
	
	public static Drawable loadFullImage(Context context, String path) {
		File fImg = new File(path);
		if (!fImg.exists()) {
			return null;
		}
		return loadImage(context, path, 1, 480, 260);
	}

	private static Drawable loadImage(Context context, String path, int size, int maxWidth, int maxHeight) {
		BitmapFactory.Options bop = new BitmapFactory.Options();
		bop.inSampleSize = size;
		return new BitmapDrawable(context.getResources(), BitmapFactory.decodeFile(path, bop));
	}
	
	public static int dipToPx(float density, int dip) {
		return (int) (dip * density + 0.5f);
	}
	
	

//	private static Drawable loadGifImage(Context context, String path, int size, int maxWidth, int maxHeight) {
//		Uri gif_uri = Uri.parse("file://" + path);
//		ContentResolver cr = context.getContentResolver();
//		Bitmap bmp = null;
//		BitmapFactory.Options bop = new BitmapFactory.Options();
//		bop.inSampleSize = size;
//		try {
//			bmp = BitmapFactory.decodeStream(cr.openInputStream(gif_uri), null, bop);
//		} catch (FileNotFoundException e) {
//			return null;
//		}
//		return new BitmapDrawable(context.getResources(), bmp);
//	}

	public static Bitmap doMatrix(Bitmap bmp, int margin, int maxWidth, int maxHeight) {
		if (bmp == null) {
			return null;
		}
		int width = bmp.getWidth();
		int height = bmp.getHeight();
		@SuppressWarnings("unused")
		int tmpWidth = width, tmpHeight = height;
		int px = margin;
		float scale = 0, scale2 = 0;
		Matrix matrix = new Matrix();

		if (width > (maxWidth)) {
			scale = ((float) (maxWidth) / width);
		}

		tmpWidth *= scale;
		tmpHeight *= scale;

		if (tmpHeight > (maxHeight - (px * 2))) {
			scale2 = ((float) (maxHeight - (px * 2)) / tmpHeight);
			scale *= scale2;
		}

		tmpWidth = (int) (width * scale);
		tmpHeight = (int) (height * scale);

		if (scale == 0) {
			matrix.postScale(1, 1);
		} else {
			matrix.postScale(scale, scale);
		}
		return Bitmap.createBitmap(bmp, 0, 0, width, height, matrix, true);
	}
}
