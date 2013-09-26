package com.rarnu.utils;

import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;

public class DrawableUtils {

    public static final int TEXT_COLOR_PRIMARY = android.R.attr.textColorPrimary;
    public static final int TEXT_COLOR_SECONDARY = android.R.attr.textColorSecondary;
    public static final int SELECTOR_BACKGROUND_COLOR = android.R.attr.listSelector;
    public static final int DETAILS_ELEMENT_BACKGROUND = android.R.attr.detailsElementBackground;

    public static ColorStateList getSystemAttrColor(Context context, int attr) {
        TypedArray a = context.obtainStyledAttributes(new int[]{attr});
        ColorStateList color = a.getColorStateList(a.getIndex(0));
        a.recycle();
        return color;
    }

    public static ColorStateList getTextColorPrimary(Context context) {
        return getSystemAttrColor(context, TEXT_COLOR_PRIMARY);
    }

    public static ColorStateList getTextColorSecondary(Context context) {
        return getSystemAttrColor(context, TEXT_COLOR_SECONDARY);
    }

    public static ColorStateList getSelectorBackgroundColor(Context context) {
        return getSystemAttrColor(context, SELECTOR_BACKGROUND_COLOR);
    }

    public static Drawable getSystemAttrDrawable(Context context, int attr) {
        TypedArray a = context.obtainStyledAttributes(new int[]{attr});
        Drawable d = a.getDrawable(a.getIndex(0));
        a.recycle();
        return d;
    }
}
