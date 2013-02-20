package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;

public class ColorUtils {

	public static ColorStateList getSystemAttrColor(Context context, int attr) {
		TypedArray a = context.obtainStyledAttributes(new int[] { attr });
		return a.getColorStateList(a.getIndex(0));

	}
}
