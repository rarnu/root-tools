package com.rarnu.tools.neo.utils

import android.content.Context
import android.content.res.ColorStateList
import android.graphics.drawable.Drawable

/**
 * Created by rarnu on 3/23/16.
 */
object DrawableUtils {

    val TEXT_COLOR_PRIMARY = android.R.attr.textColorPrimary
    val TEXT_COLOR_SECONDARY = android.R.attr.textColorSecondary
    val SELECTOR_BACKGROUND_COLOR = android.R.attr.listSelector
    val DETAILS_ELEMENT_BACKGROUND = android.R.attr.detailsElementBackground

    fun getSystemAttrColor(context: Context?, attr: Int): ColorStateList? {
        val a = context?.obtainStyledAttributes(intArrayOf(attr))
        val color = a?.getColorStateList(a.getIndex(0))
        a?.recycle()
        return color
    }

    fun getTextColorPrimary(context: Context?): ColorStateList? = getSystemAttrColor(context, TEXT_COLOR_PRIMARY)

    fun getTextColorSecondary(context: Context?): ColorStateList? = getSystemAttrColor(context, TEXT_COLOR_SECONDARY)

    fun getSelectorBackgroundColor(context: Context?): ColorStateList? = getSystemAttrColor(context, SELECTOR_BACKGROUND_COLOR)

    fun getDetailsElementBackground(context: Context?): Drawable? = getSystemAttrDrawable(context, DETAILS_ELEMENT_BACKGROUND)

    fun getSystemAttrDrawable(context: Context?, attr: Int): Drawable? {
        val a = context?.obtainStyledAttributes(intArrayOf(attr))
        val d = a?.getDrawable(a.getIndex(0))
        a?.recycle()
        return d
    }

}