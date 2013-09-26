package com.rarnu.devlib.component.tools;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Rect;
import android.os.Build;
import android.util.AttributeSet;
import android.util.FloatMath;
import android.view.*;
import android.view.animation.Interpolator;
import android.widget.Scroller;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.devlib.component.intf.OnClosedListener;
import com.rarnu.devlib.component.intf.OnOpenedListener;
import com.rarnu.devlib.component.intf.OnPageChangeListener;

import java.util.ArrayList;
import java.util.List;

public class CustomViewAbove extends ViewGroup {

    private static final boolean USE_CACHE = false;
    private static final int MAX_SETTLE_DURATION = 600;
    private static final int MIN_DISTANCE_FOR_FLING = 25;
    private static final Interpolator sInterpolator = new Interpolator() {
        public float getInterpolation(float t) {
            t -= 1.0f;
            return t * t * t * t * t + 1.0f;
        }
    };
    private static final int INVALID_POINTER = -1;
    protected int mActivePointerId = INVALID_POINTER;
    protected VelocityTracker mVelocityTracker;
    protected int mMaximumVelocity;
    protected int mTouchMode = SlidingMenu.TOUCHMODE_MARGIN;
    private View mContent;
    private int mCurItem;
    private Scroller mScroller;
    private boolean mScrollingCacheEnabled;
    private boolean mScrolling;
    private boolean mIsBeingDragged;
    private boolean mIsUnableToDrag;
    private int mTouchSlop;
    private float mInitialMotionX;
    private float mLastMotionX;
    private float mLastMotionY;
    private int mMinimumVelocity;
    private int mFlingDistance;
    private CustomViewBehind mViewBehind;
    private boolean mEnabled = true;
    private OnPageChangeListener mOnPageChangeListener;
    private OnPageChangeListener mInternalPageChangeListener;
    private OnClosedListener mClosedListener;
    private OnOpenedListener mOpenedListener;
    private List<View> mIgnoredViews = new ArrayList<View>();
    private boolean mQuickReturn = false;
    private float mScrollX = 0.0f;

    public CustomViewAbove(Context context) {
        this(context, null);
    }

    public CustomViewAbove(Context context, AttributeSet attrs) {
        super(context, attrs);
        initCustomViewAbove();
    }

    void initCustomViewAbove() {
        setWillNotDraw(false);
        setDescendantFocusability(FOCUS_AFTER_DESCENDANTS);
        setFocusable(true);
        final Context context = getContext();
        mScroller = new Scroller(context, sInterpolator);
        final ViewConfiguration configuration = ViewConfiguration.get(context);
        mTouchSlop = configuration.getScaledPagingTouchSlop();
        mMinimumVelocity = configuration.getScaledMinimumFlingVelocity();
        mMaximumVelocity = configuration.getScaledMaximumFlingVelocity();
        setInternalPageChangeListener(new SimpleOnPageChangeListener() {
            public void onPageSelected(int position) {
                if (mViewBehind != null) {
                    switch (position) {
                        case 0:
                        case 2:
                            mViewBehind.setChildrenEnabled(true);
                            break;
                        case 1:
                            mViewBehind.setChildrenEnabled(false);
                            break;
                    }
                }
            }
        });

        final float density = context.getResources().getDisplayMetrics().density;
        mFlingDistance = (int) (MIN_DISTANCE_FOR_FLING * density);
    }

    public void setCurrentItem(int item, boolean smoothScroll) {
        setCurrentItemInternal(item, smoothScroll, false);
    }

    public int getCurrentItem() {
        return mCurItem;
    }

    public void setCurrentItem(int item) {
        setCurrentItemInternal(item, true, false);
    }

    void setCurrentItemInternal(int item, boolean smoothScroll, boolean always) {
        setCurrentItemInternal(item, smoothScroll, always, 0);
    }

    void setCurrentItemInternal(int item, boolean smoothScroll, boolean always, int velocity) {
        if (!always && mCurItem == item) {
            setScrollingCacheEnabled(false);
            return;
        }

        item = mViewBehind.getMenuPage(item);

        final boolean dispatchSelected = mCurItem != item;
        mCurItem = item;
        final int destX = getDestScrollX(mCurItem);
        if (dispatchSelected && mOnPageChangeListener != null) {
            mOnPageChangeListener.onPageSelected(item);
        }
        if (dispatchSelected && mInternalPageChangeListener != null) {
            mInternalPageChangeListener.onPageSelected(item);
        }
        if (smoothScroll) {
            smoothScrollTo(destX, 0, velocity);
        } else {
            completeScroll();
            scrollTo(destX, 0);
        }
    }

    public void setOnPageChangeListener(OnPageChangeListener listener) {
        mOnPageChangeListener = listener;
    }

    public void setOnOpenedListener(OnOpenedListener l) {
        mOpenedListener = l;
    }

    public void setOnClosedListener(OnClosedListener l) {
        mClosedListener = l;
    }

    OnPageChangeListener setInternalPageChangeListener(OnPageChangeListener listener) {
        OnPageChangeListener oldListener = mInternalPageChangeListener;
        mInternalPageChangeListener = listener;
        return oldListener;
    }

    public void addIgnoredView(View v) {
        if (!mIgnoredViews.contains(v)) {
            mIgnoredViews.add(v);
        }
    }

    public void removeIgnoredView(View v) {
        mIgnoredViews.remove(v);
    }

    public void clearIgnoredViews() {
        mIgnoredViews.clear();
    }

    float distanceInfluenceForSnapDuration(float f) {
        f -= 0.5f;
        f *= 0.3f * Math.PI / 2.0f;
        return (float) FloatMath.sin(f);
    }

    public int getDestScrollX(int page) {
        switch (page) {
            case 0:
            case 2:
                return mViewBehind.getMenuLeft(mContent, page);
            case 1:
                return mContent.getLeft();
        }
        return 0;
    }

    private int getLeftBound() {
        return mViewBehind.getAbsLeftBound(mContent);
    }

    private int getRightBound() {
        return mViewBehind.getAbsRightBound(mContent);
    }

    public int getContentLeft() {
        return mContent.getLeft() + mContent.getPaddingLeft();
    }

    public boolean isMenuOpen() {
        return mCurItem == 0 || mCurItem == 2;
    }

    private boolean isInIgnoredView(MotionEvent ev) {
        Rect rect = new Rect();
        for (View v : mIgnoredViews) {
            v.getHitRect(rect);
            if (rect.contains((int) ev.getX(), (int) ev.getY())) {
                return true;
            }
        }
        return false;
    }

    public int getBehindWidth() {
        if (mViewBehind == null) {
            return 0;
        } else {
            return mViewBehind.getBehindWidth();
        }
    }

    public int getChildWidth(int i) {
        switch (i) {
            case 0:
                return getBehindWidth();
            case 1:
                return mContent.getWidth();
            default:
                return 0;
        }
    }

    public boolean isSlidingEnabled() {
        return mEnabled;
    }

    public void setSlidingEnabled(boolean b) {
        mEnabled = b;
    }

    void smoothScrollTo(int x, int y) {
        smoothScrollTo(x, y, 0);
    }

    void smoothScrollTo(int x, int y, int velocity) {
        if (getChildCount() == 0) {
            setScrollingCacheEnabled(false);
            return;
        }
        int sx = getScrollX();
        int sy = getScrollY();
        int dx = x - sx;
        int dy = y - sy;
        if (dx == 0 && dy == 0) {
            completeScroll();
            if (isMenuOpen()) {
                if (mOpenedListener != null)
                    mOpenedListener.onOpened();
            } else {
                if (mClosedListener != null)
                    mClosedListener.onClosed();
            }
            return;
        }

        setScrollingCacheEnabled(true);
        mScrolling = true;

        final int width = getBehindWidth();
        final int halfWidth = width / 2;
        final float distanceRatio = Math.min(1f, 1.0f * Math.abs(dx) / width);
        final float distance = halfWidth + halfWidth * distanceInfluenceForSnapDuration(distanceRatio);

        int duration = 0;
        velocity = Math.abs(velocity);
        if (velocity > 0) {
            duration = 4 * Math.round(1000 * Math.abs(distance / velocity));
        } else {
            final float pageDelta = (float) Math.abs(dx) / width;
            duration = (int) ((pageDelta + 1) * 100);
            duration = MAX_SETTLE_DURATION;
        }
        duration = Math.min(duration, MAX_SETTLE_DURATION);

        mScroller.startScroll(sx, sy, dx, dy, duration);
        invalidate();
    }

    public View getContent() {
        return mContent;
    }

    public void setContent(View v) {
        if (mContent != null) {
            this.removeView(mContent);
        }
        mContent = v;
        addView(mContent);
    }

    public void setCustomViewBehind(CustomViewBehind cvb) {
        mViewBehind = cvb;
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {

        int width = getDefaultSize(0, widthMeasureSpec);
        int height = getDefaultSize(0, heightMeasureSpec);
        setMeasuredDimension(width, height);

        final int contentWidth = getChildMeasureSpec(widthMeasureSpec, 0, width);
        final int contentHeight = getChildMeasureSpec(heightMeasureSpec, 0, height);
        mContent.measure(contentWidth, contentHeight);
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);

        if (w != oldw) {
            completeScroll();
            scrollTo(getDestScrollX(mCurItem), getScrollY());
        }
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        final int width = r - l;
        final int height = b - t;
        mContent.layout(0, 0, width, height);
    }

    public void setAboveOffset(int i) {
        mContent.setPadding(i, mContent.getPaddingTop(), mContent.getPaddingRight(), mContent.getPaddingBottom());
    }

    @Override
    public void computeScroll() {
        if (!mScroller.isFinished()) {
            if (mScroller.computeScrollOffset()) {
                int oldX = getScrollX();
                int oldY = getScrollY();
                int x = mScroller.getCurrX();
                int y = mScroller.getCurrY();

                if (oldX != x || oldY != y) {
                    scrollTo(x, y);
                    pageScrolled(x);
                }

                invalidate();
                return;
            }
        }
        completeScroll();
    }

    private void pageScrolled(int xpos) {
        final int widthWithMargin = getWidth();
        final int position = xpos / widthWithMargin;
        final int offsetPixels = xpos % widthWithMargin;
        final float offset = (float) offsetPixels / widthWithMargin;

        onPageScrolled(position, offset, offsetPixels);
    }

    protected void onPageScrolled(int position, float offset, int offsetPixels) {
        if (mOnPageChangeListener != null) {
            mOnPageChangeListener.onPageScrolled(position, offset, offsetPixels);
        }
        if (mInternalPageChangeListener != null) {
            mInternalPageChangeListener.onPageScrolled(position, offset, offsetPixels);
        }
    }

    private void completeScroll() {
        boolean needPopulate = mScrolling;
        if (needPopulate) {
            setScrollingCacheEnabled(false);
            mScroller.abortAnimation();
            int oldX = getScrollX();
            int oldY = getScrollY();
            int x = mScroller.getCurrX();
            int y = mScroller.getCurrY();
            if (oldX != x || oldY != y) {
                scrollTo(x, y);
            }
            if (isMenuOpen()) {
                if (mOpenedListener != null)
                    mOpenedListener.onOpened();
            } else {
                if (mClosedListener != null)
                    mClosedListener.onClosed();
            }
        }
        mScrolling = false;
    }

    public int getTouchMode() {
        return mTouchMode;
    }

    public void setTouchMode(int i) {
        mTouchMode = i;
    }

    private boolean thisTouchAllowed(MotionEvent ev) {
        int x = (int) (ev.getX() + mScrollX);
        if (isMenuOpen()) {
            return mViewBehind.menuOpenTouchAllowed(mContent, mCurItem, x);
        } else {
            switch (mTouchMode) {
                case SlidingMenu.TOUCHMODE_FULLSCREEN:
                    return !isInIgnoredView(ev);
                case SlidingMenu.TOUCHMODE_NONE:
                    return false;
                case SlidingMenu.TOUCHMODE_MARGIN:
                    return mViewBehind.marginTouchAllowed(mContent, x);
            }
        }
        return false;
    }

    private boolean thisSlideAllowed(float dx) {
        boolean allowed = false;
        if (isMenuOpen()) {
            allowed = mViewBehind.menuOpenSlideAllowed(dx);
        } else {
            allowed = mViewBehind.menuClosedSlideAllowed(dx);
        }

        return allowed;
    }

    private int getPointerIndex(MotionEvent ev, int id) {
        int activePointerIndex = ev.findPointerIndex(id);
        if (activePointerIndex == -1) {
            mActivePointerId = INVALID_POINTER;
        }
        return activePointerIndex;
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {

        if (!mEnabled) {
            return false;
        }
        final int action = ev.getAction() & MotionEvent.ACTION_MASK;

        if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP || (action != MotionEvent.ACTION_DOWN && mIsUnableToDrag)) {
            endDrag();
            return false;
        }

        switch (action) {
            case MotionEvent.ACTION_MOVE:
                determineDrag(ev);
                break;
            case MotionEvent.ACTION_DOWN:
                int index = ev.getActionIndex();
                mActivePointerId = ev.getPointerId(index);
                if (mActivePointerId == INVALID_POINTER) {
                    break;
                }
                mLastMotionX = mInitialMotionX = ev.getX(index);
                mLastMotionY = ev.getY(index);
                if (thisTouchAllowed(ev)) {
                    mIsBeingDragged = false;
                    mIsUnableToDrag = false;
                    if (isMenuOpen() && mViewBehind.menuTouchInQuickReturn(mContent, mCurItem, ev.getX() + mScrollX)) {
                        mQuickReturn = true;
                    }
                } else {
                    mIsUnableToDrag = true;
                }
                break;
            case MotionEvent.ACTION_POINTER_UP:
                onSecondaryPointerUp(ev);
                break;
        }

        if (!mIsBeingDragged) {
            if (mVelocityTracker == null) {
                mVelocityTracker = VelocityTracker.obtain();
            }
            mVelocityTracker.addMovement(ev);
        }
        return mIsBeingDragged || mQuickReturn;
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {

        if (!mEnabled) {
            return false;
        }
        if (!mIsBeingDragged && !thisTouchAllowed(ev)) {
            return false;
        }

        final int action = ev.getAction();

        if (mVelocityTracker == null) {
            mVelocityTracker = VelocityTracker.obtain();
        }
        mVelocityTracker.addMovement(ev);

        switch (action & MotionEvent.ACTION_MASK) {
            case MotionEvent.ACTION_DOWN:
                completeScroll();
                int index = ev.getActionIndex();
                mActivePointerId = ev.getPointerId(index);
                mLastMotionX = mInitialMotionX = ev.getX();
                break;
            case MotionEvent.ACTION_MOVE:
                if (!mIsBeingDragged) {
                    determineDrag(ev);
                    if (mIsUnableToDrag) {
                        return false;
                    }
                }
                if (mIsBeingDragged) {
                    final int activePointerIndex = getPointerIndex(ev, mActivePointerId);
                    if (mActivePointerId == INVALID_POINTER) {
                        break;
                    }
                    final float x = ev.getX(activePointerIndex);
                    final float deltaX = mLastMotionX - x;
                    mLastMotionX = x;
                    float oldScrollX = getScrollX();
                    float scrollX = oldScrollX + deltaX;
                    final float leftBound = getLeftBound();
                    final float rightBound = getRightBound();
                    if (scrollX < leftBound) {
                        scrollX = leftBound;
                    } else if (scrollX > rightBound) {
                        scrollX = rightBound;
                    }
                    mLastMotionX += scrollX - (int) scrollX;
                    scrollTo((int) scrollX, getScrollY());
                    pageScrolled((int) scrollX);
                }
                break;
            case MotionEvent.ACTION_UP:
                if (mIsBeingDragged) {
                    final VelocityTracker velocityTracker = mVelocityTracker;
                    velocityTracker.computeCurrentVelocity(1000, mMaximumVelocity);

                    int initialVelocity = (int) velocityTracker.getXVelocity(mActivePointerId);
                    final int scrollX = getScrollX();

                    final float pageOffset = (float) (scrollX - getDestScrollX(mCurItem)) / getBehindWidth();
                    final int activePointerIndex = getPointerIndex(ev, mActivePointerId);
                    if (mActivePointerId != INVALID_POINTER) {
                        final float x = ev.getX(activePointerIndex);
                        final int totalDelta = (int) (x - mInitialMotionX);
                        int nextPage = determineTargetPage(pageOffset, initialVelocity, totalDelta);
                        setCurrentItemInternal(nextPage, true, true, initialVelocity);
                    } else {
                        setCurrentItemInternal(mCurItem, true, true, initialVelocity);
                    }
                    mActivePointerId = INVALID_POINTER;
                    endDrag();
                } else if (mQuickReturn && mViewBehind.menuTouchInQuickReturn(mContent, mCurItem, ev.getX() + mScrollX)) {
                    setCurrentItem(1);
                    endDrag();
                }
                break;
            case MotionEvent.ACTION_CANCEL:
                if (mIsBeingDragged) {
                    setCurrentItemInternal(mCurItem, true, true);
                    mActivePointerId = INVALID_POINTER;
                    endDrag();
                }
                break;
            case MotionEvent.ACTION_POINTER_DOWN: {
                final int indexx = ev.getActionIndex();
                mLastMotionX = ev.getX(indexx);
                mActivePointerId = ev.getPointerId(indexx);
                break;
            }
            case MotionEvent.ACTION_POINTER_UP:
                onSecondaryPointerUp(ev);
                int pointerIndex = getPointerIndex(ev, mActivePointerId);
                if (mActivePointerId == INVALID_POINTER) {
                    break;
                }
                mLastMotionX = ev.getX(pointerIndex);
                break;
        }
        return true;
    }

    private void determineDrag(MotionEvent ev) {
        final int activePointerId = mActivePointerId;
        final int pointerIndex = getPointerIndex(ev, activePointerId);
        if (activePointerId == INVALID_POINTER) {
            return;
        }
        final float x = ev.getX(pointerIndex);
        final float dx = x - mLastMotionX;
        final float xDiff = Math.abs(dx);
        final float y = ev.getY(pointerIndex);
        final float dy = y - mLastMotionY;
        final float yDiff = Math.abs(dy);
        if (xDiff > (isMenuOpen() ? mTouchSlop / 2 : mTouchSlop) && xDiff > yDiff && thisSlideAllowed(dx)) {
            startDrag();
            mLastMotionX = x;
            mLastMotionY = y;
            setScrollingCacheEnabled(true);

        } else if (xDiff > mTouchSlop) {
            mIsUnableToDrag = true;
        }
    }

    @Override
    public void scrollTo(int x, int y) {
        super.scrollTo(x, y);
        mScrollX = x;
        if (mEnabled) {
            mViewBehind.scrollBehindTo(mContent, x, y);
        }
        ((SlidingMenu) getParent()).manageLayers(getPercentOpen());
    }

    private int determineTargetPage(float pageOffset, int velocity, int deltaX) {
        int targetPage = mCurItem;
        if (Math.abs(deltaX) > mFlingDistance && Math.abs(velocity) > mMinimumVelocity) {
            if (velocity > 0 && deltaX > 0) {
                targetPage -= 1;
            } else if (velocity < 0 && deltaX < 0) {
                targetPage += 1;
            }
        } else {
            targetPage = (int) Math.round(mCurItem + pageOffset);
        }
        return targetPage;
    }

    protected float getPercentOpen() {
        return Math.abs(mScrollX - mContent.getLeft()) / getBehindWidth();
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {
        super.dispatchDraw(canvas);
        mViewBehind.drawShadow(mContent, canvas);
        mViewBehind.drawFade(mContent, canvas, getPercentOpen());
        mViewBehind.drawSelector(mContent, canvas, getPercentOpen());
    }

    private void onSecondaryPointerUp(MotionEvent ev) {

        final int pointerIndex = ev.getActionIndex();
        final int pointerId = ev.getPointerId(pointerIndex);
        if (pointerId == mActivePointerId) {

            final int newPointerIndex = pointerIndex == 0 ? 1 : 0;
            mLastMotionX = ev.getX(newPointerIndex);
            mActivePointerId = ev.getPointerId(newPointerIndex);
            if (mVelocityTracker != null) {
                mVelocityTracker.clear();
            }
        }
    }

    private void startDrag() {
        mIsBeingDragged = true;
        mQuickReturn = false;
    }

    private void endDrag() {
        mQuickReturn = false;
        mIsBeingDragged = false;
        mIsUnableToDrag = false;
        mActivePointerId = INVALID_POINTER;

        if (mVelocityTracker != null) {
            mVelocityTracker.recycle();
            mVelocityTracker = null;
        }
    }

    private void setScrollingCacheEnabled(boolean enabled) {
        if (mScrollingCacheEnabled != enabled) {
            mScrollingCacheEnabled = enabled;
            if (USE_CACHE) {
                final int size = getChildCount();
                for (int i = 0; i < size; ++i) {
                    final View child = getChildAt(i);
                    if (child.getVisibility() != GONE) {
                        child.setDrawingCacheEnabled(enabled);
                    }
                }
            }
        }
    }

    protected boolean canScroll(View v, boolean checkV, int dx, int x, int y) {
        if (v instanceof ViewGroup) {
            final ViewGroup group = (ViewGroup) v;
            final int scrollX = v.getScrollX();
            final int scrollY = v.getScrollY();
            final int count = group.getChildCount();

            for (int i = count - 1; i >= 0; i--) {
                final View child = group.getChildAt(i);
                if (x + scrollX >= child.getLeft() && x + scrollX < child.getRight() && y + scrollY >= child.getTop() && y + scrollY < child.getBottom() && canScroll(child, true, dx, x + scrollX - child.getLeft(), y + scrollY - child.getTop())) {
                    return true;
                }
            }
        }

        return checkV && v.canScrollHorizontally(-dx);
    }

    @Override
    public boolean dispatchKeyEvent(KeyEvent event) {

        return super.dispatchKeyEvent(event) || executeKeyEvent(event);
    }

    public boolean executeKeyEvent(KeyEvent event) {
        boolean handled = false;
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            switch (event.getKeyCode()) {
                case KeyEvent.KEYCODE_DPAD_LEFT:
                    handled = arrowScroll(FOCUS_LEFT);
                    break;
                case KeyEvent.KEYCODE_DPAD_RIGHT:
                    handled = arrowScroll(FOCUS_RIGHT);
                    break;
                case KeyEvent.KEYCODE_TAB:
                    if (Build.VERSION.SDK_INT >= 11) {

                        if (event.hasNoModifiers()) {
                            handled = arrowScroll(FOCUS_FORWARD);
                        } else if (event.hasModifiers(KeyEvent.META_SHIFT_ON)) {
                            handled = arrowScroll(FOCUS_BACKWARD);
                        }
                    }
                    break;
            }
        }
        return handled;
    }

    public boolean arrowScroll(int direction) {
        View currentFocused = findFocus();
        if (currentFocused == this)
            currentFocused = null;

        boolean handled = false;

        View nextFocused = FocusFinder.getInstance().findNextFocus(this, currentFocused, direction);
        if (nextFocused != null && nextFocused != currentFocused) {
            if (direction == View.FOCUS_LEFT) {
                handled = nextFocused.requestFocus();
            } else if (direction == View.FOCUS_RIGHT) {

                if (currentFocused != null && nextFocused.getLeft() <= currentFocused.getLeft()) {
                    handled = pageRight();
                } else {
                    handled = nextFocused.requestFocus();
                }
            }
        } else if (direction == FOCUS_LEFT || direction == FOCUS_BACKWARD) {

            handled = pageLeft();
        } else if (direction == FOCUS_RIGHT || direction == FOCUS_FORWARD) {

            handled = pageRight();
        }
        if (handled) {
            playSoundEffect(SoundEffectConstants.getContantForFocusDirection(direction));
        }
        return handled;
    }

    boolean pageLeft() {
        if (mCurItem > 0) {
            setCurrentItem(mCurItem - 1, true);
            return true;
        }
        return false;
    }

    boolean pageRight() {
        if (mCurItem < 1) {
            setCurrentItem(mCurItem + 1, true);
            return true;
        }
        return false;
    }

    public static class SimpleOnPageChangeListener implements OnPageChangeListener {

        public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
        }

        public void onPageSelected(int position) {
        }

        public void onPageScrollStateChanged(int state) {
        }

    }

}
