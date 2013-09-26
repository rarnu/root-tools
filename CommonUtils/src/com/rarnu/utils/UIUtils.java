package com.rarnu.utils;

import android.app.Activity;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.PointF;
import android.graphics.Rect;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.GridView;
import android.widget.ListView;

public class UIUtils {

    public static final int ACTIONBAR_HEIGHT = android.R.attr.actionBarSize;
    private static Context context = null;
    private static DisplayMetrics dm = null;
    private static boolean followSystemBackground = true;

    public static DisplayMetrics getDM() {
        return dm;
    }

    public static float getDensity() {
        return dm.density;
    }

    public static void initDisplayMetrics(Context ctx, WindowManager wm, boolean isFollowSystemBackground) {
        if (context == null) {
            context = ctx;
        }

        dm = new DisplayMetrics();
        wm.getDefaultDisplay().getMetrics(dm);
        followSystemBackground = isFollowSystemBackground;

    }

    public static boolean touchInDialog(Activity activity, MotionEvent e) {
        int leftW, rightW, topH, bottomH;
        leftW = 8;
        rightW = dm.widthPixels - leftW;
        topH = 0;
        bottomH = 450;
        return ((e.getX() > leftW) && (e.getX() < rightW) && (e.getY() > topH) && (e.getY() < bottomH));
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
        return new PointF((dm.widthPixels / 4) + 0.09f, (dm.heightPixels / 4 * 3) + 0.09f);
    }

    public static PointF getRightBottomPoint() {
        return new PointF((dm.widthPixels / 4 * 3) + 0.09f, (dm.heightPixels / 4 * 3) + 0.09f);
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

    public static int getStatusbarHeight(Activity activity) {
        int statusHeight = 0;
        Rect localRect = new Rect();
        activity.getWindow().getDecorView().getWindowVisibleDisplayFrame(localRect);
        statusHeight = localRect.top;
        if (statusHeight == 0) {
            try {
                Class<?> localClass = Class.forName("com.android.internal.R$dimen");
                Object localObject = localClass.newInstance();
                int i5 = Integer.parseInt(localClass.getField("status_bar_height").get(localObject).toString());
                statusHeight = activity.getResources().getDimensionPixelSize(i5);
            } catch (Exception e) {

            }
        }
        return statusHeight;
    }

    public static void setActivitySizePos(Activity activity, int x, int y, int width, int height) {
        WindowManager.LayoutParams p = activity.getWindow().getAttributes();
        p.x = x;
        p.y = y;
        p.width = width;
        p.height = height;
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

    public static int countViewAdvWidth(int count, int innerMargin, int outerMargin) {
        int width = dm.widthPixels - (outerMargin * 2);
        width = width - (innerMargin * (count - 1));
        width = width / count;
        return width;
    }

    public static int countViewAdvWidthByFrame(int count, int innerMargin, int outerMargin, int frameWidth) {
        int width = frameWidth - (outerMargin * 2);
        width = width - (innerMargin * (count - 1));
        width = width / count;
        return width;
    }

    public static int countViewAdvHeight(int count, int innerMargin, int outerMargin) {
        int height = dm.heightPixels - (outerMargin * 2);
        height = height - (innerMargin * (count - 1));
        height = height / count;
        return height;
    }

    public static int countViewAdvHeightByFrame(int count, int innerMargin, int outerMargin, int frameHeight) {
        int height = frameHeight - (outerMargin * 2);
        height = height - (innerMargin * (count - 1));
        height = height / count;
        return height;
    }

    public static int getWidth() {
        return dm.widthPixels;
    }

    public static int getHeight() {
        return dm.heightPixels;
    }

    public static void setActivitySizeX(Activity a, int size) {
        WindowManager.LayoutParams lp = a.getWindow().getAttributes();
        lp.width = size;
        a.getWindow().setAttributes(lp);
    }

    public static void setActivitySizeY(Activity a, int size) {
        WindowManager.LayoutParams lp = a.getWindow().getAttributes();
        lp.height = size;
        a.getWindow().setAttributes(lp);
    }

    public static void setActivityPercentX(Activity a, float percent) {
        WindowManager.LayoutParams lp = a.getWindow().getAttributes();
        lp.width = (int) (getWidth() * percent / 100);
        a.getWindow().setAttributes(lp);
    }

    public static void setActivityPercentY(Activity a, float percent) {
        WindowManager.LayoutParams lp = a.getWindow().getAttributes();
        lp.height = (int) (getHeight() * percent / 100);
        a.getWindow().setAttributes(lp);
    }

    public static void setViewSizeX(View v, int size) {
        ViewGroup.LayoutParams lp = v.getLayoutParams();
        lp.width = size;
        v.setLayoutParams(lp);
    }

    public static void setViewSizeY(View v, int size) {
        ViewGroup.LayoutParams lp = v.getLayoutParams();
        lp.height = size;
        v.setLayoutParams(lp);
    }

    public static void setViewPercentX(View v, float percent) {
        ViewGroup.LayoutParams lp = v.getLayoutParams();
        lp.width = (int) (getWidth() * percent / 100);
        v.setLayoutParams(lp);
    }

    public static void setViewPercentY(View v, float percent) {
        ViewGroup.LayoutParams lp = v.getLayoutParams();
        lp.height = (int) (getHeight() * percent / 100);
        v.setLayoutParams(lp);
    }

    public static void setViewPercentXByFrame(View v, float percent, float frameXPercent) {
        ViewGroup.LayoutParams lp = v.getLayoutParams();
        lp.width = (int) ((getWidth() * frameXPercent / 100) * percent / 100);
        v.setLayoutParams(lp);
    }

    public static void setViewPercentYByFrame(View v, float percent, float frameYPercent) {
        ViewGroup.LayoutParams lp = v.getLayoutParams();
        lp.height = (int) ((getHeight() * frameYPercent / 100) * percent / 100);
        v.setLayoutParams(lp);
    }

    public static void setViewMarginPercent(View v, float marginLeft, float marginTop, float marginRight, float marginBottom)
            throws Exception {
        ViewGroup.LayoutParams lp = v.getLayoutParams();
        if (lp instanceof ViewGroup.MarginLayoutParams) {
            ViewGroup.MarginLayoutParams mlp = (ViewGroup.MarginLayoutParams) lp;
            mlp.leftMargin = (int) (getWidth() * marginLeft / 100);
            mlp.topMargin = (int) (getHeight() * marginTop / 100);
            mlp.rightMargin = (int) (getWidth() * marginRight / 100);
            mlp.bottomMargin = (int) (getHeight() * marginBottom / 100);
            v.setLayoutParams(mlp);
        } else {
            throw new Exception(context.getString(R.string.exception_not_margin_layout_param));
        }
    }

    public static void setViewMarginSize(View v, int marginLeft, int marginTop, int marginRight, int marginBottom) throws Exception {
        ViewGroup.LayoutParams lp = v.getLayoutParams();
        if (lp instanceof ViewGroup.MarginLayoutParams) {
            ViewGroup.MarginLayoutParams mlp = (ViewGroup.MarginLayoutParams) lp;
            mlp.leftMargin = marginLeft;
            mlp.topMargin = marginTop;
            mlp.rightMargin = marginRight;
            mlp.bottomMargin = marginBottom;
            v.setLayoutParams(mlp);
        } else {
            throw new Exception(context.getString(R.string.exception_not_margin_layout_param));
        }
    }

    public static void setViewPaddingPercent(View v, float paddingLeft, float paddingTop, float paddingRight, float paddingBottom) {
        int pLeft = (int) (getWidth() * paddingLeft / 100);
        int pTop = (int) (getHeight() * paddingTop / 100);
        int pRight = (int) (getWidth() * paddingRight / 100);
        int pBottom = (int) (getHeight() * paddingBottom / 100);
        v.setPadding(pLeft, pTop, pRight, pBottom);
    }

    public static void makeListViewFullSize(ListView lv, int itemHeight) {
        int itemCount = lv.getAdapter().getCount();
        int divider = lv.getDividerHeight();
        int height = (itemHeight + divider) * itemCount;
        ViewGroup.LayoutParams lp = lv.getLayoutParams();
        lp.height = height;
        lv.setLayoutParams(lp);
    }

    public static void makeGridViewFullSize(GridView gv, int itemHeight, int rowNum) {
        int itemCount = gv.getAdapter().getCount();
        int lines = (int) (itemCount / rowNum);
        if (itemCount % rowNum != 0) {
            lines++;
        }
        ViewGroup.LayoutParams lp = gv.getLayoutParams();
        lp.height = lines * itemHeight;
        gv.setLayoutParams(lp);

    }

    public static int getActionBarHeight() {
        TypedArray a = context.obtainStyledAttributes(new int[]{ACTIONBAR_HEIGHT});
        int ret = a.getDimensionPixelSize(0, -1);
        a.recycle();
        return ret;
    }

    public static boolean isFollowSystemBackground() {
        return followSystemBackground;
    }

    public static void setFollowSystemBackground(boolean isFollowSystemBackground) {
        followSystemBackground = isFollowSystemBackground;
    }
}
