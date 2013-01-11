package com.rarnu.vim.emotion.utils;

import java.io.InputStream;

import android.content.Context;
import android.graphics.drawable.Drawable;

public class MiscUtils {

	public static Drawable getBitmapByAssets(Context context, String uri) {
		Drawable drawable = null;
		InputStream in = null;
		try {
			in = context.getResources().getAssets().open(uri);
			drawable = Drawable.createFromResourceStream(context.getResources(), null, in, null);
		} catch (Exception e) {

		} finally {
			try {
				in.close();
			} catch (Exception ex) {

			}
		}
		
		return drawable;
	}

}
