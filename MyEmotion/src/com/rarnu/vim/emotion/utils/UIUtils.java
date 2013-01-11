package com.rarnu.vim.emotion.utils;

import android.app.Activity;
import android.content.Context;
import android.graphics.PointF;
import android.graphics.drawable.Drawable;
import android.util.DisplayMetrics;
import android.view.Display;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;

public class UIUtils {

	private static DisplayMetrics dm = null;

	public static void initDisplayMetrics(WindowManager wm) {
		if (dm == null) {
			dm = new DisplayMetrics();
			wm.getDefaultDisplay().getMetrics(dm);
		}
	}

	public static boolean touchInView(View v, MotionEvent e) {
		return false;
	}
	
	public static boolean touchInDialog(Activity activity, MotionEvent e) {
		// WindowManager.LayoutParams wlp =
		// activity.getWindow().getAttributes();
		int leftW, rightW, topH, bottomH;

		// if (wlp.width > 0 && wlp.height > 0) {
		// leftW = (dm.widthPixels - wlp.width) / 2;
		// rightW = dm.widthPixels - leftW;
		// topH = (dm.heightPixels - wlp.height) / 2;
		// bottomH = dm.heightPixels - topH;
		// } else {
		leftW = 8; // (dm.widthPixels - 16) / 2;
		rightW = dm.widthPixels - leftW;
		topH = 0; // (dm.heightPixels - 80) / 2;
		bottomH = 450;
		// }
		return ((e.getX() > leftW) && (e.getX() < rightW) && (e.getY() > topH) && (e
				.getY() < bottomH));
	}

	public static boolean isScreenCenter(MotionEvent e) {
		boolean ret = true;
		if (e.getX() < (dm.widthPixels / 2 - 25)) {
			ret = false;
		}
		if (e.getX() > (dm.widthPixels / 2 + 25)) {
			ret = false;
		}
		if (e.getY() < (dm.heightPixels / 2 - 25)) {
			ret = false;
		}
		if (e.getY() > (dm.heightPixels / 2 + 25)) {
			ret = false;
		}
		return ret;
	}

	public static PointF getLeftBottomPoint() {
		return new PointF((dm.widthPixels / 4) + 0.09f,
				(dm.heightPixels / 4 * 3) + 0.09f);
	}

	public static PointF getRightBottomPoint() {
		return new PointF((dm.widthPixels / 4 * 3) + 0.09f,
				(dm.heightPixels / 4 * 3) + 0.09f);
	}

	public static PointF getLeftPoint() {
		return new PointF(20, dm.heightPixels / 2);
	}

	public static PointF getRightPoint() {
		return new PointF(dm.widthPixels - 20, dm.heightPixels / 2);
	}

	public static boolean isTouchLeft(MotionEvent e) {
		return (e.getX() < (dm.widthPixels / 2));
	}

	@SuppressWarnings("deprecation")
	public static int getStatusbarHeight(Context context) {
		Drawable ico = context.getResources().getDrawable(
				android.R.drawable.stat_sys_phone_call);
		return ico.getIntrinsicHeight();
	}

	@SuppressWarnings("deprecation")
	public static void setActivitySizePos(Activity activity) {
		WindowManager m = activity.getWindowManager();
		Display d = m.getDefaultDisplay();
		WindowManager.LayoutParams p = activity.getWindow().getAttributes();
		p.y = 4;
		p.height = (int) (d.getHeight() - 72);
		activity.getWindow().setAttributes(p);
	}

	public static int dipToPx(int dip) {
		if (dm == null) {
			return -1;
		}
		return (int) (dip * dm.density + 0.5f);
	}

	public static float pxToScaledPx(int px) {
		if (dm == null) {
			return -1;
		}
		return px / dm.density;
	}

	public static int scaledPxToPx(float scaledPx) {
		if (dm == null) {
			return -1;
		}
		return (int) (scaledPx * dm.density);
	}

	public static int getButtonAdvWidth(int count, int margin) {
		int width = dm.widthPixels;
		width = width - (margin * (count + 1));
		width = width / count;
		return width;

	}
	
	public static int getWidth() {
		return dm.widthPixels;
	}

	public static int getHeight() {
		return dm.heightPixels;
	}

}
