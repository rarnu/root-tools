package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.*;
import android.widget.Scroller;
import com.rarnu.devlib.R;
import com.rarnu.devlib.component.intf.OnScreenChangeListener;
import com.rarnu.devlib.component.intf.OnScreenTouchListener;

public class VScrollLayout extends ViewGroup {

    private static final int TOUCH_STATE_REST = 0;
    private static final int TOUCH_STATE_SCROLLING = 1;
    private static final int SNAP_VELOCITY = 600;
    private boolean enableScroll = true;
    private Scroller mScroller;
    private VelocityTracker mVelocityTracker;
    private int mCurScreen;
    private int mDefaultScreen = 0;
    private OnScreenChangeListener screenChangeListener;
    private OnScreenTouchListener touchListener;
    private int mTouchState = TOUCH_STATE_REST;
    private int mTouchSlop;
    private float mLastMotionY;

    public VScrollLayout(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public VScrollLayout(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);

        mScroller = new Scroller(context);

        mCurScreen = mDefaultScreen;
        mTouchSlop = ViewConfiguration.get(getContext()).getScaledTouchSlop() * 4;

        setTouchState(TOUCH_STATE_REST);
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {

        int childTop = 0;
        final int childCount = getChildCount();

        for (int i = 0; i < childCount; i++) {
            final View childView = getChildAt(i);
            if (childView.getVisibility() != View.GONE) {
                final int childHeight = childView.getMeasuredHeight();
                childView.layout(0, childTop, childView.getMeasuredWidth(), childTop + childHeight);
                childTop += childHeight;
            }
        }

    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);

        final int height = MeasureSpec.getSize(heightMeasureSpec);

        final int widthMode = MeasureSpec.getMode(widthMeasureSpec);
        if (widthMode != MeasureSpec.EXACTLY) {
            throw new IllegalStateException(getContext().getString(R.string.exception_not_exactly_mode));
        }

        final int heightMode = MeasureSpec.getMode(heightMeasureSpec);
        if (heightMode != MeasureSpec.EXACTLY) {
            throw new IllegalStateException(getContext().getString(R.string.exception_not_exactly_mode));
        }

        final int count = getChildCount();
        for (int i = 0; i < count; i++) {
            getChildAt(i).measure(widthMeasureSpec, heightMeasureSpec);
        }

        scrollTo(0, mCurScreen * height);
    }

    public void snapToDestination() {
        final int screenHeight = getHeight();
        final int destScreen = (getScrollY() + screenHeight / 2) / screenHeight;
        snapToScreen(destScreen);
    }

    public void snapToScreen(int whichScreen) {

        whichScreen = Math.max(0, Math.min(whichScreen, getChildCount() - 1));
        if (getScrollY() != (whichScreen * getHeight())) {

            final int delta = whichScreen * getHeight() - getScrollY();

            mScroller.startScroll(0, getScrollY(), 0, delta, Math.abs(delta) * 2);
            mCurScreen = whichScreen;
            invalidate();

            if (screenChangeListener != null) {
                screenChangeListener.onScreenChange(this, mCurScreen);
            }
        }
    }

    public void setToScreen(int whichScreen) {
        whichScreen = Math.max(0, Math.min(whichScreen, getChildCount() - 1));
        mCurScreen = whichScreen;
        scrollTo(0, whichScreen * getHeight());
    }

    public int getCurScreen() {
        return mCurScreen;
    }

    @Override
    public void computeScroll() {

        if (mScroller.computeScrollOffset()) {
            scrollTo(mScroller.getCurrX(), mScroller.getCurrY());
            postInvalidate();
        }
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {

        if (!enableScroll) {
            return super.onTouchEvent(event);
        }
        if (mVelocityTracker == null) {
            mVelocityTracker = VelocityTracker.obtain();
        }
        mVelocityTracker.addMovement(event);

        final int action = event.getAction();
        final float y = event.getY();

        switch (action) {
            case MotionEvent.ACTION_DOWN:

                if (!mScroller.isFinished()) {
                    mScroller.abortAnimation();
                }
                mLastMotionY = y;
                return true;

            case MotionEvent.ACTION_MOVE:
                int deltaY = (int) (mLastMotionY - y);

                if (Math.abs(deltaY) > 10) {
                    setTouchState(TOUCH_STATE_SCROLLING);
                }
                mLastMotionY = y;

                scrollBy(0, deltaY);
                return true;

            case MotionEvent.ACTION_UP:

                final VelocityTracker velocityTracker = mVelocityTracker;
                velocityTracker.computeCurrentVelocity(1000);
                int velocityY = (int) velocityTracker.getYVelocity();

                if (velocityY > SNAP_VELOCITY && mCurScreen > 0) {
                    snapToScreen(mCurScreen - 1);
                } else if (velocityY < -SNAP_VELOCITY && mCurScreen < getChildCount() - 1) {
                    snapToScreen(mCurScreen + 1);
                } else {
                    snapToDestination();
                }

                if (mVelocityTracker != null) {
                    mVelocityTracker.recycle();
                    mVelocityTracker = null;
                }

                setTouchState(TOUCH_STATE_REST);
                return true;
            case MotionEvent.ACTION_CANCEL:
                setTouchState(TOUCH_STATE_REST);
                return true;
        }

        return false;
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {

        if (!enableScroll) {
            return super.onInterceptTouchEvent(ev);
        }
        final int action = ev.getAction();
        if ((action == MotionEvent.ACTION_MOVE) && (mTouchState != TOUCH_STATE_REST)) {
            return true;
        }

        final float y = ev.getY();

        switch (action) {
            case MotionEvent.ACTION_MOVE:
                final int xDiff = (int) Math.abs(mLastMotionY - y);
                if (xDiff > mTouchSlop) {
                    setTouchState(TOUCH_STATE_SCROLLING);
                }
                break;

            case MotionEvent.ACTION_DOWN:
                mLastMotionY = y;
                setTouchState(mScroller.isFinished() ? TOUCH_STATE_REST : TOUCH_STATE_SCROLLING);
                break;

            case MotionEvent.ACTION_CANCEL:
            case MotionEvent.ACTION_UP:
                setTouchState(TOUCH_STATE_REST);
                break;
        }

        return mTouchState != TOUCH_STATE_REST;
    }

    private void setTouchState(int stat) {
        mTouchState = stat;
        if (touchListener != null) {
            if (mTouchState == TOUCH_STATE_SCROLLING) {
                touchListener.onActionScrolling(this);
            }
            if (mTouchState == TOUCH_STATE_REST) {
                touchListener.onActionReset(this);
            }
        }
    }

    public OnScreenChangeListener getOnScreenChangeListener() {
        return screenChangeListener;
    }

    public void setOnScreenChangeListener(
            OnScreenChangeListener screenChangeListener) {
        this.screenChangeListener = screenChangeListener;
    }

    public OnScreenTouchListener getScreenTouchListener() {
        return touchListener;
    }

    public void setOnScreenTouchListener(OnScreenTouchListener touchListener) {
        this.touchListener = touchListener;
    }

    public boolean isEnableScroll() {
        return enableScroll;
    }

    public void setEnableScroll(boolean enableScroll) {
        this.enableScroll = enableScroll;
    }

}
