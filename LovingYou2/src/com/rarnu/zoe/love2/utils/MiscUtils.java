package com.rarnu.zoe.love2.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Calendar;

import android.app.Activity;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.telephony.TelephonyManager;
import android.view.inputmethod.InputMethodManager;

import com.rarnu.zoe.love2.common.Consts;

public class MiscUtils {

	public static Calendar loadTimeMillis(long time) {
		Calendar c = Calendar.getInstance();
		c.setTimeInMillis(time);
		return c;
	}

	public static Calendar loadDefaultCalendar(int hour) {
		Calendar cDef = Calendar.getInstance();
		cDef.set(Calendar.HOUR_OF_DAY, hour);
		cDef.set(Calendar.MINUTE, 0);
		return cDef;
	}

	public static void hideInput(Activity activity) {
		if (activity.getCurrentFocus() != null) {
			InputMethodManager inputMethodManager = (InputMethodManager) activity
					.getSystemService(Context.INPUT_METHOD_SERVICE);
			inputMethodManager.hideSoftInputFromWindow(activity
					.getCurrentFocus().getWindowToken(),
					InputMethodManager.HIDE_NOT_ALWAYS);
		}
	}

	public static Uri saveLocalFile(Context context, int index) {
		BitmapFactory.Options bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;
		Bitmap bmp = BitmapFactory.decodeResource(context.getResources(),
				Consts.bpImgs[index], bop);
		String filename = DownloadUtils.SAVE_PATH
				+ String.format("%d.png", index);

		FileOutputStream out = null;
		try {
			out = new FileOutputStream(filename);
			bmp.compress(Bitmap.CompressFormat.PNG, 100, out);
			out.flush();
		} catch (Exception e) {
		} finally {
			try {
				out.close();
			} catch (IOException e) {
			}
			out = null;
		}
		return Uri.fromFile(new File(filename));
	}

	public static String getDeviceUniqueId(Context context) {
		TelephonyManager tm = (TelephonyManager) context
				.getSystemService(Context.TELEPHONY_SERVICE);
		return tm.getDeviceId() + "_" + tm.getSubscriberId();
	}

	public static String getDeviceModule() {
		String module = android.os.Build.MODEL + " "
				+ android.os.Build.VERSION.RELEASE;
		return module;
	}
}
