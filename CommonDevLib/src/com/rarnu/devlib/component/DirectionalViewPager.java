package com.rarnu.devlib.component;

import android.content.Context;
import android.database.DataSetObserver;
import android.os.Build;
import android.os.Parcel;
import android.os.Parcelable;
import android.support.v4.os.ParcelableCompat;
import android.support.v4.os.ParcelableCompatCreatorCallbacks;
import android.support.v4.view.*;
import android.util.AttributeSet;
import android.view.*;
import android.widget.Scroller;

import java.util.ArrayList;

public class DirectionalViewPager extends ViewPager {
    private static final String XML_NS = "http://schemas.android.com/apk/res/android";

    private static final boolean USE_CACHE = false;

    public static final int HORIZONTAL = 0;
    public static final int VERTICAL = 1;

    static class ItemInfo {
        Object object;
        int position;
        boolean scrolling;
    }

    private final ArrayList<ItemInfo> mItems = new ArrayList<ItemInfo>();

    private PagerAdapter mAdapter;
    private int mCurItem;   // Index of currently displayed page.
    private int mRestoredCurItem = -1;
    private Parcelable mRestoredAdapterState = null;
    private ClassLoader mRestoredClassLoader = null;
    private Scroller mScroller;

    private DirectionalViewPager.PagerObserver mObserver;
//    private VerticalViewPagerCompat.DataSetObserver mObserver;

    private int mChildWidthMeasureSpec;
    private int mChildHeightMeasureSpec;
    private boolean mInLayout;

    private boolean mScrollingCacheEnabled;

    private boolean mPopulatePending;
    private boolean mScrolling;

    private boolean mIsBeingDragged;
    private boolean mIsUnableToDrag;
    private int mTouchSlop;
    private float mInitialMotion;

    private float mLastMotionX;
    private float mLastMotionY;
    private int mOrientation = HORIZONTAL;

    private int mActivePointerId = INVALID_POINTER;

    private static final int INVALID_POINTER = -1;

    private VelocityTracker mVelocityTracker;
    private int mMinimumVelocity;
    private int mMaximumVelocity;

    private OnPageChangeListener mOnPageChangeListener;

    private int mScrollState = SCROLL_STATE_IDLE;

    public DirectionalViewPager(Context context) {
        super(context);
        initViewPager();
    }

    public DirectionalViewPager(Context context, AttributeSet attrs) {
        super(context, attrs);
        initViewPager();

        int orientation = attrs.getAttributeIntValue(XML_NS, "orientation", -1);
        if (orientation != -1) {
            setOrientation(orientation);
        }
    }

    void initViewPager() {
        setWillNotDraw(false);
        mScroller = new Scroller(getContext());
        final ViewConfiguration configuration = ViewConfiguration.get(getContext());
        mTouchSlop = ViewConfigurationCompat.getScaledPagingTouchSlop(configuration);
        mMinimumVelocity = configuration.getScaledMinimumFlingVelocity();
        mMaximumVelocity = configuration.getScaledMaximumFlingVelocity();
    }

    private void setScrollState(int newState) {
        if (mScrollState == newState) {
            return;
        }

        mScrollState = newState;
        if (mOnPageChangeListener != null) {
            mOnPageChangeListener.onPageScrollStateChanged(newState);
        }
    }

    public void setAdapter(PagerAdapter adapter) {
//        if (mAdapter != null) {
//            VerticalViewPagerCompat.setDataSetObserver(mAdapter, null);
//        }

        mAdapter = adapter;

        if (mAdapter != null) {
            if (mObserver == null) {
                mObserver = new PagerObserver();
            }
            mAdapter.registerDataSetObserver(mObserver);
//            VerticalViewPagerCompat.setDataSetObserver(mAdapter, mObserver);
            mPopulatePending = false;
            if (mRestoredCurItem >= 0) {
                mAdapter.restoreState(mRestoredAdapterState, mRestoredClassLoader);
                setCurrentItemInternal(mRestoredCurItem, false, true);
                mRestoredCurItem = -1;
                mRestoredAdapterState = null;
                mRestoredClassLoader = null;
            } else {
                populate();
            }
        }
    }

    public PagerAdapter getAdapter() {
        return mAdapter;
    }

    public void setCurrentItem(int item) {
        mPopulatePending = false;
        setCurrentItemInternal(item, true, false);
    }

    void setCurrentItemInternal(int item, boolean smoothScroll, boolean always) {
        if (mAdapter == null || mAdapter.getCount() <= 0) {
            setScrollingCacheEnabled(false);
            return;
        }
        if (!always && mCurItem == item && mItems.size() != 0) {
            setScrollingCacheEnabled(false);
            return;
        }
        if (item < 0) {
            item = 0;
        } else if (item >= mAdapter.getCount()) {
            item = mAdapter.getCount() - 1;
        }
        if (item > (mCurItem + 1) || item < (mCurItem - 1)) {
            for (int i = 0; i < mItems.size(); i++) {
                mItems.get(i).scrolling = true;
            }
        }
        final boolean dispatchSelected = mCurItem != item;
        mCurItem = item;
        populate();
        if (smoothScroll) {
            if (mOrientation == HORIZONTAL) {
                smoothScrollTo(getWidth() * item, 0);
            } else {
                smoothScrollTo(0, getHeight() * item);
            }
            if (dispatchSelected && mOnPageChangeListener != null) {
                mOnPageChangeListener.onPageSelected(item);
            }
        } else {
            if (dispatchSelected && mOnPageChangeListener != null) {
                mOnPageChangeListener.onPageSelected(item);
            }
            completeScroll();
            if (mOrientation == HORIZONTAL) {
                scrollTo(getWidth() * item, 0);
            } else {
                scrollTo(0, getHeight() * item);
            }
        }
    }

    public void setOnPageChangeListener(OnPageChangeListener listener) {
        mOnPageChangeListener = listener;
    }

    void smoothScrollTo(int x, int y) {
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
            return;
        }

        setScrollingCacheEnabled(true);
        mScrolling = true;
        setScrollState(SCROLL_STATE_SETTLING);
        mScroller.startScroll(sx, sy, dx, dy);
        invalidate();
    }

    void addNewItem(int position, int index) {
        ItemInfo ii = new ItemInfo();
        ii.position = position;
        ii.object = mAdapter.instantiateItem(this, position);
        if (index < 0) {
            mItems.add(ii);
        } else {
            mItems.add(index, ii);
        }
    }

    void dataSetChanged() {
        // This method only gets called if our observer is attached, so mAdapter is non-null.

        boolean needPopulate = mItems.isEmpty() && mAdapter.getCount() > 0;
        int newCurrItem = -1;

        for (int i = 0; i < mItems.size(); i++) {
            final ItemInfo ii = mItems.get(i);
            final int newPos = mAdapter.getItemPosition(ii.object);

            if (newPos == PagerAdapter.POSITION_UNCHANGED) {
                continue;
            }

            if (newPos == PagerAdapter.POSITION_NONE) {
                mItems.remove(i);
                i--;
                mAdapter.destroyItem(this, ii.position, ii.object);
                needPopulate = true;

                if (mCurItem == ii.position) {
                    // Keep the current item in the valid range
                    newCurrItem = Math.max(0, Math.min(mCurItem, mAdapter.getCount() - 1));
                }
                continue;
            }

            if (ii.position != newPos) {
                if (ii.position == mCurItem) {
                    // Our current item changed position. Follow it.
                    newCurrItem = newPos;
                }

                ii.position = newPos;
                needPopulate = true;
            }
        }

        if (newCurrItem >= 0) {
            setCurrentItemInternal(newCurrItem, false, true);
            needPopulate = true;
        }
        if (needPopulate) {
            populate();
            requestLayout();
        }
    }

    void populate() {
        if (mAdapter == null) {
            return;
        }

        if (mPopulatePending) {
            return;
        }

        if (getWindowToken() == null) {
            return;
        }

        mAdapter.startUpdate(this);

        final int startPos = mCurItem > 0 ? mCurItem - 1 : mCurItem;
        final int count = mAdapter.getCount();
        final int endPos = mCurItem < (count - 1) ? mCurItem + 1 : count - 1;

        int lastPos = -1;
        for (int i = 0; i < mItems.size(); i++) {
            ItemInfo ii = mItems.get(i);
            if ((ii.position < startPos || ii.position > endPos) && !ii.scrolling) {
                mItems.remove(i);
                i--;
                mAdapter.destroyItem(this, ii.position, ii.object);
            } else if (lastPos < endPos && ii.position > startPos) {

                lastPos++;
                if (lastPos < startPos) {
                    lastPos = startPos;
                }
                while (lastPos <= endPos && lastPos < ii.position) {
                    addNewItem(lastPos, i);
                    lastPos++;
                    i++;
                }
            }
            lastPos = ii.position;
        }

        lastPos = mItems.size() > 0 ? mItems.get(mItems.size() - 1).position : -1;
        if (lastPos < endPos) {
            lastPos++;
            lastPos = lastPos > startPos ? lastPos : startPos;
            while (lastPos <= endPos) {
                addNewItem(lastPos, -1);
                lastPos++;
            }
        }
        mAdapter.finishUpdate(this);
    }

    public static class SavedState extends BaseSavedState {
        int position;
        Parcelable adapterState;
        ClassLoader loader;

        public SavedState(Parcelable superState) {
            super(superState);
        }

        @Override
        public void writeToParcel(Parcel out, int flags) {
            super.writeToParcel(out, flags);
            out.writeInt(position);
            out.writeParcelable(adapterState, flags);
        }

        @Override
        public String toString() {
            return "FragmentPager.SavedState{"
                    + Integer.toHexString(System.identityHashCode(this))
                    + " position=" + position + "}";
        }

        public static final Parcelable.Creator<SavedState> CREATOR
                = ParcelableCompat.newCreator(new ParcelableCompatCreatorCallbacks<SavedState>() {
            @Override
            public SavedState createFromParcel(Parcel in, ClassLoader loader) {
                return new SavedState(in, loader);
            }

            @Override
            public SavedState[] newArray(int size) {
                return new SavedState[size];
            }
        });

        SavedState(Parcel in, ClassLoader loader) {
            super(in);
            if (loader == null) {
                loader = getClass().getClassLoader();
            }
            position = in.readInt();
            adapterState = in.readParcelable(loader);
            this.loader = loader;
        }
    }

    @Override
    public Parcelable onSaveInstanceState() {
        Parcelable superState = super.onSaveInstanceState();
        SavedState ss = new SavedState(superState);
        ss.position = mCurItem;
        ss.adapterState = mAdapter.saveState();
        return ss;
    }

    @Override
    public void onRestoreInstanceState(Parcelable state) {
        if (!(state instanceof SavedState)) {
            super.onRestoreInstanceState(state);
            return;
        }

        SavedState ss = (SavedState) state;
        super.onRestoreInstanceState(ss.getSuperState());

        if (mAdapter != null) {
            mAdapter.restoreState(ss.adapterState, ss.loader);
            setCurrentItemInternal(ss.position, false, true);
        } else {
            mRestoredCurItem = ss.position;
            mRestoredAdapterState = ss.adapterState;
            mRestoredClassLoader = ss.loader;
        }
    }

    public int getOrientation() {
        return mOrientation;
    }

    public void setOrientation(int orientation) {
        switch (orientation) {
            case HORIZONTAL:
            case VERTICAL:
                break;

            default:
                throw new IllegalArgumentException("Only HORIZONTAL and VERTICAL are valid orientations.");
        }

        if (orientation == mOrientation) {
            return;
        }

        completeScroll();
        mInitialMotion = 0;
        mLastMotionX = 0;
        mLastMotionY = 0;
        if (mVelocityTracker != null) {
            mVelocityTracker.clear();
        }

        mOrientation = orientation;
        if (mOrientation == HORIZONTAL) {
            scrollTo(mCurItem * getWidth(), 0);
        } else {
            scrollTo(0, mCurItem * getHeight());
        }
        requestLayout();
    }

    @Override
    public void addView(View child, int index, ViewGroup.LayoutParams params) {
        if (mInLayout) {
            addViewInLayout(child, index, params);
            child.measure(mChildWidthMeasureSpec, mChildHeightMeasureSpec);
        } else {
            super.addView(child, index, params);
        }

        if (USE_CACHE) {
            if (child.getVisibility() != GONE) {
                child.setDrawingCacheEnabled(mScrollingCacheEnabled);
            } else {
                child.setDrawingCacheEnabled(false);
            }
        }
    }

    ItemInfo infoForChild(View child) {
        for (int i = 0; i < mItems.size(); i++) {
            ItemInfo ii = mItems.get(i);
            if (mAdapter.isViewFromObject(child, ii.object)) {
                return ii;
            }
        }
        return null;
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (mAdapter != null) {
            populate();
        }
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {

        setMeasuredDimension(getDefaultSize(0, widthMeasureSpec),
                getDefaultSize(0, heightMeasureSpec));

        mChildWidthMeasureSpec = MeasureSpec.makeMeasureSpec(getMeasuredWidth() -
                getPaddingLeft() - getPaddingRight(), MeasureSpec.EXACTLY);
        mChildHeightMeasureSpec = MeasureSpec.makeMeasureSpec(getMeasuredHeight() -
                getPaddingTop() - getPaddingBottom(), MeasureSpec.EXACTLY);

        mInLayout = true;
        populate();
        mInLayout = false;

        final int size = getChildCount();
        for (int i = 0; i < size; ++i) {
            final View child = getChildAt(i);
            if (child.getVisibility() != GONE) {
                child.measure(mChildWidthMeasureSpec, mChildHeightMeasureSpec);
            }
        }
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);

        if (mOrientation == HORIZONTAL) {
            int scrollPos = mCurItem * w;
            if (scrollPos != getScrollX()) {
                completeScroll();
                scrollTo(scrollPos, getScrollY());
            }
        } else {
            int scrollPos = mCurItem * h;
            if (scrollPos != getScrollY()) {
                completeScroll();
                scrollTo(getScrollX(), scrollPos);
            }
        }
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        mInLayout = true;
        populate();
        mInLayout = false;

        final int count = getChildCount();
        final int size = (mOrientation == HORIZONTAL) ? r - l : b - t;

        for (int i = 0; i < count; i++) {
            View child = getChildAt(i);
            ItemInfo ii;
            if (child.getVisibility() != GONE && (ii = infoForChild(child)) != null) {
                int off = size * ii.position;
                int childLeft = getPaddingLeft();
                int childTop = getPaddingTop();
                if (mOrientation == HORIZONTAL) {
                    childLeft += off;
                } else {
                    childTop += off;
                }
                child.layout(childLeft, childTop,
                        childLeft + child.getMeasuredWidth(),
                        childTop + child.getMeasuredHeight());
            }
        }
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
                }

                if (mOnPageChangeListener != null) {
                    int size;
                    int value;
                    if (mOrientation == HORIZONTAL) {
                        size = getWidth();
                        value = x;
                    } else {
                        size = getHeight();
                        value = y;
                    }

                    final int position = value / size;
                    final int offsetPixels = value % size;
                    final float offset = (float) offsetPixels / size;
                    mOnPageChangeListener.onPageScrolled(position, offset, offsetPixels);
                }

                invalidate();
                return;
            }
        }

        completeScroll();
    }

    private void completeScroll() {
        boolean needPopulate;
        if ((needPopulate = mScrolling)) {

            setScrollingCacheEnabled(false);
            mScroller.abortAnimation();
            int oldX = getScrollX();
            int oldY = getScrollY();
            int x = mScroller.getCurrX();
            int y = mScroller.getCurrY();
            if (oldX != x || oldY != y) {
                scrollTo(x, y);
            }
            setScrollState(SCROLL_STATE_IDLE);
        }
        mPopulatePending = false;
        mScrolling = false;
        for (int i = 0; i < mItems.size(); i++) {
            ItemInfo ii = mItems.get(i);
            if (ii.scrolling) {
                needPopulate = true;
                ii.scrolling = false;
            }
        }
        if (needPopulate) {
            populate();
        }
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {

        final int action = ev.getAction() & MotionEventCompat.ACTION_MASK;

        if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP) {

            mIsBeingDragged = false;
            mIsUnableToDrag = false;
            mActivePointerId = INVALID_POINTER;
            return false;
        }

        if (action != MotionEvent.ACTION_DOWN) {
            if (mIsBeingDragged) {
                return true;
            }
            if (mIsUnableToDrag) {
                return false;
            }
        }

        switch (action) {
            case MotionEvent.ACTION_MOVE: {

                final int activePointerId = mActivePointerId;
                if (activePointerId == INVALID_POINTER && Build.VERSION.SDK_INT > Build.VERSION_CODES.DONUT) {
                    break;
                }

                final int pointerIndex = MotionEventCompat.findPointerIndex(ev, activePointerId);
                final float x = MotionEventCompat.getX(ev, pointerIndex);
                final float y = MotionEventCompat.getY(ev, pointerIndex);
                final float xDiff = Math.abs(x - mLastMotionX);
                final float yDiff = Math.abs(y - mLastMotionY);
                float primaryDiff;
                float secondaryDiff;

                if (mOrientation == HORIZONTAL) {
                    primaryDiff = xDiff;
                    secondaryDiff = yDiff;
                } else {
                    primaryDiff = yDiff;
                    secondaryDiff = xDiff;
                }

                if (primaryDiff > mTouchSlop && primaryDiff > secondaryDiff) {
                    mIsBeingDragged = true;
                    setScrollState(SCROLL_STATE_DRAGGING);
                    if (mOrientation == HORIZONTAL) {
                        mLastMotionX = x;
                    } else {
                        mLastMotionY = y;
                    }
                    setScrollingCacheEnabled(true);
                } else {
                    if (secondaryDiff > mTouchSlop) {
                        mIsUnableToDrag = true;
                    }
                }
                break;
            }

            case MotionEvent.ACTION_DOWN: {

                if (mOrientation == HORIZONTAL) {
                    mLastMotionX = mInitialMotion = ev.getX();
                    mLastMotionY = ev.getY();
                } else {
                    mLastMotionX = ev.getX();
                    mLastMotionY = mInitialMotion = ev.getY();
                }
                mActivePointerId = MotionEventCompat.getPointerId(ev, 0);

                if (mScrollState == SCROLL_STATE_SETTLING) {
                    mIsBeingDragged = true;
                    mIsUnableToDrag = false;
                    setScrollState(SCROLL_STATE_DRAGGING);
                } else {
                    completeScroll();
                    mIsBeingDragged = false;
                    mIsUnableToDrag = false;
                }

                break;
            }

            case MotionEventCompat.ACTION_POINTER_UP:
                onSecondaryPointerUp(ev);
                break;
        }

        return mIsBeingDragged;
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {

        if (ev.getAction() == MotionEvent.ACTION_DOWN && ev.getEdgeFlags() != 0) {
            return false;
        }

        if (mAdapter == null || mAdapter.getCount() == 0) {
            return false;
        }

        if (mVelocityTracker == null) {
            mVelocityTracker = VelocityTracker.obtain();
        }
        mVelocityTracker.addMovement(ev);

        final int action = ev.getAction();

        switch (action & MotionEventCompat.ACTION_MASK) {
            case MotionEvent.ACTION_DOWN: {

                completeScroll();

                if (mOrientation == HORIZONTAL) {
                    mLastMotionX = mInitialMotion = ev.getX();
                } else {
                    mLastMotionY = mInitialMotion = ev.getY();
                }
                mActivePointerId = MotionEventCompat.getPointerId(ev, 0);
                break;
            }
            case MotionEvent.ACTION_MOVE:
                if (!mIsBeingDragged) {
                    final int pointerIndex = MotionEventCompat.findPointerIndex(ev, mActivePointerId);
                    final float x = MotionEventCompat.getX(ev, pointerIndex);
                    final float y = MotionEventCompat.getY(ev, pointerIndex);
                    final float xDiff = Math.abs(x - mLastMotionX);
                    final float yDiff = Math.abs(y - mLastMotionY);
                    float primaryDiff;
                    float secondaryDiff;

                    if (mOrientation == HORIZONTAL) {
                        primaryDiff = xDiff;
                        secondaryDiff = yDiff;
                    } else {
                        primaryDiff = yDiff;
                        secondaryDiff = xDiff;
                    }

                    if (primaryDiff > mTouchSlop && primaryDiff > secondaryDiff) {
                        mIsBeingDragged = true;
                        if (mOrientation == HORIZONTAL) {
                            mLastMotionX = x;
                        } else {
                            mLastMotionY = y;
                        }
                        setScrollState(SCROLL_STATE_DRAGGING);
                        setScrollingCacheEnabled(true);
                    }
                }
                if (mIsBeingDragged) {
                    final int activePointerIndex = MotionEventCompat.findPointerIndex(
                            ev, mActivePointerId);
                    final float x = MotionEventCompat.getX(ev, activePointerIndex);
                    final float y = MotionEventCompat.getY(ev, activePointerIndex);

                    int size;
                    float scroll;

                    if (mOrientation == HORIZONTAL) {
                        size = getWidth();
                        scroll = getScrollX() + (mLastMotionX - x);
                        mLastMotionX = x;
                    } else {
                        size = getHeight();
                        scroll = getScrollY() + (mLastMotionY - y);
                        mLastMotionY = y;
                    }

                    final float lowerBound = Math.max(0, (mCurItem - 1) * size);
                    final float upperBound =
                            Math.min(mCurItem + 1, mAdapter.getCount() - 1) * size;
                    if (scroll < lowerBound) {
                        scroll = lowerBound;
                    } else if (scroll > upperBound) {
                        scroll = upperBound;
                    }
                    if (mOrientation == HORIZONTAL) {
                        mLastMotionX += scroll - (int) scroll;
                        scrollTo((int) scroll, getScrollY());
                    } else {
                        mLastMotionY += scroll - (int) scroll;
                        scrollTo(getScrollX(), (int) scroll);
                    }
                    if (mOnPageChangeListener != null) {
                        final int position = (int) scroll / size;
                        final int positionOffsetPixels = (int) scroll % size;
                        final float positionOffset = (float) positionOffsetPixels / size;
                        mOnPageChangeListener.onPageScrolled(position, positionOffset,
                                positionOffsetPixels);
                    }
                }
                break;
            case MotionEvent.ACTION_UP:
                if (mIsBeingDragged) {
                    final VelocityTracker velocityTracker = mVelocityTracker;
                    velocityTracker.computeCurrentVelocity(1000, mMaximumVelocity);
                    int initialVelocity;
                    float lastMotion;
                    int sizeOverThree;

                    if (mOrientation == HORIZONTAL) {
                        initialVelocity = (int) VelocityTrackerCompat.getXVelocity(
                                velocityTracker, mActivePointerId);
                        lastMotion = mLastMotionX;
                        sizeOverThree = getWidth() / 3;
                    } else {
                        initialVelocity = (int) VelocityTrackerCompat.getYVelocity(
                                velocityTracker, mActivePointerId);
                        lastMotion = mLastMotionY;
                        sizeOverThree = getHeight() / 3;
                    }

                    mPopulatePending = true;
                    if ((Math.abs(initialVelocity) > mMinimumVelocity)
                            || Math.abs(mInitialMotion - lastMotion) >= sizeOverThree) {
                        if (lastMotion > mInitialMotion) {
                            setCurrentItemInternal(mCurItem - 1, true, true);
                        } else {
                            setCurrentItemInternal(mCurItem + 1, true, true);
                        }
                    } else {
                        setCurrentItemInternal(mCurItem, true, true);
                    }

                    mActivePointerId = INVALID_POINTER;
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
            case MotionEventCompat.ACTION_POINTER_DOWN: {
                final int index = MotionEventCompat.getActionIndex(ev);
                if (mOrientation == HORIZONTAL) {
                    mLastMotionX = MotionEventCompat.getX(ev, index);
                } else {
                    mLastMotionY = MotionEventCompat.getY(ev, index);
                }
                mActivePointerId = MotionEventCompat.getPointerId(ev, index);
                break;
            }
            case MotionEventCompat.ACTION_POINTER_UP:
                onSecondaryPointerUp(ev);
                final int index = MotionEventCompat.findPointerIndex(ev, mActivePointerId);
                if (mOrientation == HORIZONTAL) {
                    mLastMotionX = MotionEventCompat.getX(ev, index);
                } else {
                    mLastMotionY = MotionEventCompat.getY(ev, index);
                }
                break;
        }
        return true;
    }

    private void onSecondaryPointerUp(MotionEvent ev) {
        final int pointerIndex = MotionEventCompat.getActionIndex(ev);
        final int pointerId = MotionEventCompat.getPointerId(ev, pointerIndex);
        if (pointerId == mActivePointerId) {
            final int newPointerIndex = pointerIndex == 0 ? 1 : 0;
            if (mOrientation == HORIZONTAL) {
                mLastMotionX = MotionEventCompat.getX(ev, newPointerIndex);
            } else {
                mLastMotionY = MotionEventCompat.getY(ev, newPointerIndex);
            }
            mActivePointerId = MotionEventCompat.getPointerId(ev, newPointerIndex);
            if (mVelocityTracker != null) {
                mVelocityTracker.clear();
            }
        }
    }

    private void endDrag() {
        mIsBeingDragged = false;
        mIsUnableToDrag = false;

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

    private class PagerObserver extends DataSetObserver {
        private PagerObserver() {
        }

        public void onChanged() {
            dataSetChanged();
        }

        public void onInvalidated() {

        }
    }

}