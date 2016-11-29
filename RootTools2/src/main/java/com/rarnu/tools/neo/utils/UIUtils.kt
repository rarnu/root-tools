package com.rarnu.tools.neo.utils

import android.app.Activity
import android.app.ActivityOptions
import android.content.Context
import android.os.Build
import android.util.DisplayMetrics
import android.view.*
import android.widget.GridView
import android.widget.ListView
import android.widget.SearchView

/**
 * Created by rarnu on 3/23/16.
 */
object UIUtils {

    val ACTIONBAR_HEIGHT = android.R.attr.actionBarSize
    private var context: Context? = null
    var dm: DisplayMetrics? = null
    var isFollowSystemBackground = false
    var density: Float? = null
        get() {
            return dm?.density
        }
    var width: Int? = null
        get() {
            return dm?.widthPixels
        }
    var height: Int? = null
        get() {
            return dm?.heightPixels
        }

    fun initDisplayMetrics(ctx: Context, wm: WindowManager, follow: Boolean) {
        context = ctx
        dm = DisplayMetrics()
        wm.defaultDisplay.getMetrics(dm)
        isFollowSystemBackground = follow
    }

    fun dip2px(dip: Int): Int {
        if (dm == null) {
            return -1
        }
        return (dip * dm!!.density + 0.5f).toInt()
    }

    fun px2scaled(px: Int): Float {
        if (dm == null) {
            return -1.0f
        }
        return px / dm!!.density
    }

    fun scaled2px(scaled: Float): Int {
        if (dm == null) {
            return -1
        }
        return (scaled * dm!!.density).toInt()
    }

    var actionBarHeight: Int? = null
        get() {
            val a = context?.obtainStyledAttributes(intArrayOf(ACTIONBAR_HEIGHT))
            val ret = a?.getDimensionPixelSize(0, -1)
            a?.recycle()
            return ret
        }

    var statusBarHeight: Int?= null
        get() {
            val resId = context?.resources?.getIdentifier("status_bar_height", "dimen", "android")
            val height = context?.resources?.getDimensionPixelSize(resId!!)
            return height
        }

    var navigationBarHeight: Int? = null
        get() {
            val resId = context?.resources?.getIdentifier("navigation_bar_height", "dimen", "android")
            val height = context?.resources?.getDimensionPixelSize(resId!!)
            return height
        }

    var hasNavigationBar = false
        get() {
            if (Build.MODEL.toLowerCase().contains("nexus")) {
                val hasMenuKey = ViewConfiguration.get(context).hasPermanentMenuKey()
                val hasBackKey = KeyCharacterMap.deviceHasKey(KeyEvent.KEYCODE_BACK)
                return (!hasMenuKey && !hasBackKey)
            } else {
                return false
            }
        }

    fun makeListViewFullSize(lv: ListView, itemHeight: Int) {
        val itemCount = lv.adapter.count
        val divider = lv.dividerHeight
        val height = (itemHeight + divider) * itemCount
        lv.layoutParams.height = height
    }

    fun makeGridViewFullSize(gv: GridView, itemHeight: Int, colNum: Int) {
        val itemCount = gv.adapter.count
        val lines = (itemCount / colNum).toInt() + (if (itemCount % colNum != 0) { 1 } else { 0 })
        gv.layoutParams.height = itemHeight * lines
    }

    fun setActivitySizePos(activity: Activity, x: Int, y: Int, width: Int, height: Int) {
        with(activity.window.attributes) {
            this.x = x
            this.y = y
            this.width = width
            this.height = height
        }
    }

    fun setActivityWidth(activity: Activity, width: Int) {
        activity.window.attributes.width = width
    }

    fun setActivityHeight(activity: Activity, height: Int) {
        activity.window.attributes.height = height
    }

    fun setActivityWidthPercent(activity: Activity, percent: Float) {
        activity.window.attributes.width = (width!! * percent / 100.0f).toInt()
    }

    fun setActivityHeightPercent(activity: Activity, percent: Float) {
        activity.window.attributes.height = (height!! * percent / 100.0f).toInt()
    }

    fun setViewWidth(v: View, width: Int) {
        v.layoutParams.width = width
    }

    fun setViewHeight(v: View, height: Int) {
        v.layoutParams.height = height
    }

    fun setViewWidthPercent(v: View, percent: Float) {
        v.layoutParams.width = (width!! * percent / 100.0f).toInt()
    }


    fun setViewHeightPercent(v: View, percent: Float) {
        v.layoutParams.height = (height!! * percent / 100.0f).toInt()
    }

    fun setViewMargin(v: View, left: Int, top: Int, right: Int, bottom: Int) {
        if (v.layoutParams is ViewGroup.MarginLayoutParams) {
            with(v.layoutParams as ViewGroup.MarginLayoutParams) {
                leftMargin = left
                topMargin = top
                rightMargin = right
                bottomMargin = bottom
            }
        }
    }

    fun setViewMarginPercent(v: View, left: Int, top: Int, right: Int, bottom: Int) {
        if (v.layoutParams is ViewGroup.MarginLayoutParams) {
            with(v.layoutParams as ViewGroup.MarginLayoutParams) {
                leftMargin = (UIUtils.width!! * left / 100.0f).toInt()
                topMargin = (UIUtils.height!! * top / 100.0f).toInt()
                rightMargin = (UIUtils.width!! * right / 100.0f).toInt()
                bottomMargin = (UIUtils.height!! * bottom / 100.0f).toInt()
            }
        }
    }

    fun setImmersion(activity: Activity, immersion: Boolean) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
            if (immersion) {
                with(activity.window) {
                    addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS)
                    if (hasNavigationBar) {
                        addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_NAVIGATION)
                    }
                }
            }
        }
    }

    fun setFitSystem(activity: Activity, res: Int, immersion: Boolean) {
        val v = activity.findViewById(res)
        if (v is ViewGroup) {
            setFitSystem(v, immersion)
        }
    }

    fun setFitSystem(v: ViewGroup, immersion: Boolean) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
            if (immersion) {
                v.fitsSystemWindows = true
                v.setClipToPadding(true)
            }
        }
    }

    fun setSearchViewTextBackground(sv: SearchView, backgroundRes: Int) = try {
        val clz = sv.javaClass
        val fSearchPlate = clz.getDeclaredField("mSearchPlate")
        fSearchPlate.isAccessible =  true
        val vSearchPlate = fSearchPlate.get(sv) as View
        vSearchPlate.setBackgroundResource(backgroundRes)
        val fSubmitArea = clz.getDeclaredField("submit_area")
        fSubmitArea.isAccessible = true
        val vSubmitArea = fSubmitArea.get(sv) as View
        vSubmitArea.setBackgroundResource(backgroundRes)
    } catch(e: Exception) {

    }

}