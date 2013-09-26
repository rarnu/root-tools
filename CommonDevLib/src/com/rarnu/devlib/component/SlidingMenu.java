package com.rarnu.devlib.component;

import android.app.Activity;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Point;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.os.Handler;
import android.os.Parcel;
import android.os.Parcelable;
import android.util.AttributeSet;
import android.view.*;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;
import com.rarnu.devlib.R;
import com.rarnu.devlib.component.intf.*;
import com.rarnu.devlib.component.tools.CustomViewAbove;
import com.rarnu.devlib.component.tools.CustomViewBehind;

import java.lang.reflect.Method;

public class SlidingMenu extends RelativeLayout {

    public static final int SLIDING_WINDOW = 0;
    public static final int SLIDING_CONTENT = 1;
    public static final int TOUCHMODE_MARGIN = 0;
    public static final int TOUCHMODE_FULLSCREEN = 1;
    public static final int TOUCHMODE_NONE = 2;
    public static final int LEFT = 0;
    public static final int RIGHT = 1;
    public static final int LEFT_RIGHT = 2;
    private boolean mActionbarOverlay = false;
    private CustomViewAbove mViewAbove;
    private CustomViewBehind mViewBehind;
    private OnOpenListener mOpenListener;
    private OnCloseListener mCloseListener;
    private Handler mHandler = new Handler();

    public SlidingMenu(Context context) {
        this(context, null);
    }

    public SlidingMenu(Activity activity, int slideStyle) {
        this(activity, null);
        this.attachToActivity(activity, slideStyle);
    }

    public SlidingMenu(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SlidingMenu(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);

        LayoutParams behindParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        mViewBehind = new CustomViewBehind(context);
        addView(mViewBehind, behindParams);
        LayoutParams aboveParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        mViewAbove = new CustomViewAbove(context);
        addView(mViewAbove, aboveParams);

        mViewAbove.setCustomViewBehind(mViewBehind);
        mViewBehind.setCustomViewAbove(mViewAbove);
        mViewAbove.setOnPageChangeListener(new OnPageChangeListener() {
            public static final int POSITION_OPEN = 0;
            public static final int POSITION_CLOSE = 1;

            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
            }

            public void onPageSelected(int position) {
                if (position == POSITION_OPEN && mOpenListener != null) {
                    mOpenListener.onOpen();
                } else if (position == POSITION_CLOSE && mCloseListener != null) {
                    mCloseListener.onClose();
                }
            }
        });

        TypedArray ta = context.obtainStyledAttributes(attrs, R.styleable.SlidingMenu);
        int mode = ta.getInt(R.styleable.SlidingMenu_mode, LEFT);
        setMode(mode);
        int viewAbove = ta.getResourceId(R.styleable.SlidingMenu_viewAbove, -1);
        if (viewAbove != -1) {
            setContent(viewAbove);
        } else {
            setContent(new FrameLayout(context));
        }
        int viewBehind = ta.getResourceId(R.styleable.SlidingMenu_viewBehind, -1);
        if (viewBehind != -1) {
            setMenu(viewBehind);
        } else {
            setMenu(new FrameLayout(context));
        }
        int touchModeAbove = ta.getInt(R.styleable.SlidingMenu_touchModeAbove, TOUCHMODE_MARGIN);
        setTouchModeAbove(touchModeAbove);
        int touchModeBehind = ta.getInt(R.styleable.SlidingMenu_touchModeBehind, TOUCHMODE_MARGIN);
        setTouchModeBehind(touchModeBehind);

        int offsetBehind = (int) ta.getDimension(R.styleable.SlidingMenu_behindOffset, -1);
        int widthBehind = (int) ta.getDimension(R.styleable.SlidingMenu_behindWidth, -1);
        if (offsetBehind != -1 && widthBehind != -1) {
            throw new IllegalStateException("Cannot set both behindOffset and behindWidth for a SlidingMenu");
        } else if (offsetBehind != -1) {
            setBehindOffset(offsetBehind);
        } else if (widthBehind != -1) {
            setBehindWidth(widthBehind);
        } else {
            setBehindOffset(0);
        }
        float scrollOffsetBehind = ta.getFloat(R.styleable.SlidingMenu_behindScrollScale, 0.33f);
        setBehindScrollScale(scrollOffsetBehind);
        int shadowRes = ta.getResourceId(R.styleable.SlidingMenu_shadowDrawable, -1);
        if (shadowRes != -1) {
            setShadowDrawable(shadowRes);
        }
        int shadowWidth = (int) ta.getDimension(R.styleable.SlidingMenu_shadowWidth, 0);
        setShadowWidth(shadowWidth);
        boolean fadeEnabled = ta.getBoolean(R.styleable.SlidingMenu_fadeEnabled, true);
        setFadeEnabled(fadeEnabled);
        float fadeDeg = ta.getFloat(R.styleable.SlidingMenu_fadeDegree, 0.33f);
        setFadeDegree(fadeDeg);
        boolean selectorEnabled = ta.getBoolean(R.styleable.SlidingMenu_selectorEnabled, false);
        setSelectorEnabled(selectorEnabled);
        int selectorRes = ta.getResourceId(R.styleable.SlidingMenu_selectorDrawable, -1);
        if (selectorRes != -1) {
            setSelectorDrawable(selectorRes);
        }
        ta.recycle();
    }

    public void attachToActivity(Activity activity, int slideStyle) {
        attachToActivity(activity, slideStyle, false);
    }

    public void attachToActivity(Activity activity, int slideStyle, boolean actionbarOverlay) {
        if (slideStyle != SLIDING_WINDOW && slideStyle != SLIDING_CONTENT) {
            throw new IllegalArgumentException("slideStyle must be either SLIDING_WINDOW or SLIDING_CONTENT");
        }
        if (getParent() != null) {
            throw new IllegalStateException("This SlidingMenu appears to already be attached");
        }
        TypedArray a = activity.getTheme().obtainStyledAttributes(new int[]{android.R.attr.windowBackground});
        int background = a.getResourceId(0, 0);
        a.recycle();

        switch (slideStyle) {
            case SLIDING_WINDOW:
                mActionbarOverlay = false;
                ViewGroup decor = (ViewGroup) activity.getWindow().getDecorView();
                ViewGroup decorChild = (ViewGroup) decor.getChildAt(0);

                decorChild.setBackgroundResource(background);
                decor.removeView(decorChild);
                decor.addView(this);
                setContent(decorChild);
                break;
            case SLIDING_CONTENT:
                mActionbarOverlay = actionbarOverlay;

                ViewGroup contentParent = (ViewGroup) activity.findViewById(android.R.id.content);
                View content = contentParent.getChildAt(0);
                contentParent.removeView(content);
                contentParent.addView(this);
                setContent(content);

                if (content.getBackground() == null) {
                    content.setBackgroundResource(background);
                }
                break;
        }
    }

    public void setContent(int res) {
        setContent(LayoutInflater.from(getContext()).inflate(res, null));
    }

    public View getContent() {
        return mViewAbove.getContent();
    }

    public void setContent(View view) {
        mViewAbove.setContent(view);
        showContent();
    }

    public void setMenu(int res) {
        setMenu(LayoutInflater.from(getContext()).inflate(res, null));
    }

    public View getMenu() {
        return mViewBehind.getContent();
    }

    public void setMenu(View v) {
        mViewBehind.setContent(v);
    }

    public void setSecondaryMenu(int res) {
        setSecondaryMenu(LayoutInflater.from(getContext()).inflate(res, null));
    }

    public View getSecondaryMenu() {
        return mViewBehind.getSecondaryContent();
    }

    public void setSecondaryMenu(View v) {
        mViewBehind.setSecondaryContent(v);
    }

    public boolean isSlidingEnabled() {
        return mViewAbove.isSlidingEnabled();
    }

    public void setSlidingEnabled(boolean b) {
        mViewAbove.setSlidingEnabled(b);
    }

    public int getMode() {
        return mViewBehind.getMode();
    }

    public void setMode(int mode) {
        if (mode != LEFT && mode != RIGHT && mode != LEFT_RIGHT) {
            throw new IllegalStateException("SlidingMenu mode must be LEFT, RIGHT, or LEFT_RIGHT");
        }
        mViewBehind.setMode(mode);
    }

    public void setStatic(boolean b) {
        if (b) {
            setSlidingEnabled(false);
            mViewAbove.setCustomViewBehind(null);
            mViewAbove.setCurrentItem(1);

        } else {
            mViewAbove.setCurrentItem(1);
            mViewAbove.setCustomViewBehind(mViewBehind);
            setSlidingEnabled(true);
        }
    }

    public void showMenu() {
        showMenu(true);
    }

    public void showMenu(boolean animate) {
        mViewAbove.setCurrentItem(0, animate);
    }

    public void showSecondaryMenu() {
        showSecondaryMenu(true);
    }

    public void showSecondaryMenu(boolean animate) {
        mViewAbove.setCurrentItem(2, animate);
    }

    public void showContent() {
        showContent(true);
    }

    public void showContent(boolean animate) {
        mViewAbove.setCurrentItem(1, animate);
    }

    public void toggle() {
        toggle(true);
    }

    public void toggle(boolean animate) {
        if (isMenuShowing()) {
            showContent(animate);
        } else {
            showMenu(animate);
        }
    }

    public boolean isMenuShowing() {
        return mViewAbove.getCurrentItem() == 0 || mViewAbove.getCurrentItem() == 2;
    }

    public boolean isSecondaryMenuShowing() {
        return mViewAbove.getCurrentItem() == 2;
    }

    public int getBehindOffset() {
        return ((RelativeLayout.LayoutParams) mViewBehind.getLayoutParams()).rightMargin;
    }

    public void setBehindOffset(int i) {
        mViewBehind.setWidthOffset(i);
    }

    public void setBehindOffsetRes(int resID) {
        int i = (int) getContext().getResources().getDimension(resID);
        setBehindOffset(i);
    }

    public void setAboveOffset(int i) {
        mViewAbove.setAboveOffset(i);
    }

    public void setAboveOffsetRes(int resID) {
        int i = (int) getContext().getResources().getDimension(resID);
        setAboveOffset(i);
    }

    public void setBehindWidth(int i) {
        int width;
        Display display = ((WindowManager) getContext().getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay();
        try {
            Class<?> cls = Display.class;
            Class<?>[] parameterTypes = {Point.class};
            Point parameter = new Point();
            Method method = cls.getMethod("getSize", parameterTypes);
            method.invoke(display, parameter);
            width = parameter.x;
        } catch (Exception e) {
            width = display.getWidth();
        }
        setBehindOffset(width - i);
    }

    public void setBehindWidthRes(int res) {
        int i = (int) getContext().getResources().getDimension(res);
        setBehindWidth(i);
    }

    public float getBehindScrollScale() {
        return mViewBehind.getScrollScale();
    }

    public void setBehindScrollScale(float f) {
        if (f < 0 && f > 1) {
            throw new IllegalStateException("ScrollScale must be between 0 and 1");
        }
        mViewBehind.setScrollScale(f);
    }

    public void setBehindCanvasTransformer(CanvasTransformer t) {
        mViewBehind.setCanvasTransformer(t);
    }

    public int getTouchModeAbove() {
        return mViewAbove.getTouchMode();
    }

    public void setTouchModeAbove(int i) {
        if (i != TOUCHMODE_FULLSCREEN && i != TOUCHMODE_MARGIN && i != TOUCHMODE_NONE) {
            throw new IllegalStateException("TouchMode must be set to eitherTOUCHMODE_FULLSCREEN or TOUCHMODE_MARGIN or TOUCHMODE_NONE.");
        }
        mViewAbove.setTouchMode(i);
    }

    public void setTouchModeBehind(int i) {
        if (i != TOUCHMODE_FULLSCREEN && i != TOUCHMODE_MARGIN && i != TOUCHMODE_NONE) {
            throw new IllegalStateException("TouchMode must be set to eitherTOUCHMODE_FULLSCREEN or TOUCHMODE_MARGIN or TOUCHMODE_NONE.");
        }
        mViewBehind.setTouchMode(i);
    }

    public void setShadowDrawable(int resId) {
        setShadowDrawable(getContext().getResources().getDrawable(resId));
    }

    public void setShadowDrawable(Drawable d) {
        mViewBehind.setShadowDrawable(d);
    }

    public void setSecondaryShadowDrawable(int resId) {
        setSecondaryShadowDrawable(getContext().getResources().getDrawable(resId));
    }

    public void setSecondaryShadowDrawable(Drawable d) {
        mViewBehind.setSecondaryShadowDrawable(d);
    }

    public void setShadowWidthRes(int resId) {
        setShadowWidth((int) getResources().getDimension(resId));
    }

    public void setShadowWidth(int pixels) {
        mViewBehind.setShadowWidth(pixels);
    }

    public void setFadeEnabled(boolean b) {
        mViewBehind.setFadeEnabled(b);
    }

    public void setFadeDegree(float f) {
        mViewBehind.setFadeDegree(f);
    }

    public void setSelectorEnabled(boolean b) {
        mViewBehind.setSelectorEnabled(true);
    }

    public void setSelectedView(View v) {
        mViewBehind.setSelectedView(v);
    }

    public void setSelectorDrawable(int res) {
        mViewBehind.setSelectorBitmap(BitmapFactory.decodeResource(getResources(), res));
    }

    public void setSelectorBitmap(Bitmap b) {
        mViewBehind.setSelectorBitmap(b);
    }

    public void addIgnoredView(View v) {
        mViewAbove.addIgnoredView(v);
    }

    public void removeIgnoredView(View v) {
        mViewAbove.removeIgnoredView(v);
    }

    public void clearIgnoredViews() {
        mViewAbove.clearIgnoredViews();
    }

    public void setOnOpenListener(OnOpenListener listener) {
        mOpenListener = listener;
    }

    public void setOnCloseListener(OnCloseListener listener) {
        mCloseListener = listener;
    }

    public void setOnOpenedListener(OnOpenedListener listener) {
        mViewAbove.setOnOpenedListener(listener);
    }

    public void setOnClosedListener(OnClosedListener listener) {
        mViewAbove.setOnClosedListener(listener);
    }

    @Override
    protected Parcelable onSaveInstanceState() {
        Parcelable superState = super.onSaveInstanceState();
        SavedState ss = new SavedState(superState, mViewAbove.getCurrentItem());
        return ss;
    }

    @Override
    protected void onRestoreInstanceState(Parcelable state) {
        SavedState ss = (SavedState) state;
        super.onRestoreInstanceState(ss.getSuperState());
        mViewAbove.setCurrentItem(ss.getItem());
    }

    @Override
    protected boolean fitSystemWindows(Rect insets) {
        int leftPadding = insets.left;
        int rightPadding = insets.right;
        int topPadding = insets.top;
        int bottomPadding = insets.bottom;
        if (!mActionbarOverlay) {
            setPadding(leftPadding, topPadding, rightPadding, bottomPadding);
        }
        return true;
    }

    public void manageLayers(float percentOpen) {
        if (Build.VERSION.SDK_INT < 11) {
            return;
        }

        boolean layer = percentOpen > 0.0f && percentOpen < 1.0f;
        final int layerType = layer ? View.LAYER_TYPE_HARDWARE : View.LAYER_TYPE_NONE;

        if (layerType != getContent().getLayerType()) {
            mHandler.post(new Runnable() {
                public void run() {
                    getContent().setLayerType(layerType, null);
                    getMenu().setLayerType(layerType, null);
                    if (getSecondaryMenu() != null) {
                        getSecondaryMenu().setLayerType(layerType, null);
                    }
                }
            });
        }
    }

    public static class SavedState extends BaseSavedState {

        public static final Parcelable.Creator<SavedState> CREATOR = new Parcelable.Creator<SavedState>() {
            public SavedState createFromParcel(Parcel in) {
                return new SavedState(in);
            }

            public SavedState[] newArray(int size) {
                return new SavedState[size];
            }
        };
        private final int mItem;

        public SavedState(Parcelable superState, int item) {
            super(superState);
            mItem = item;
        }

        private SavedState(Parcel in) {
            super(in);
            mItem = in.readInt();
        }

        public int getItem() {
            return mItem;
        }

        public void writeToParcel(Parcel out, int flags) {
            super.writeToParcel(out, flags);
            out.writeInt(mItem);
        }

    }

}