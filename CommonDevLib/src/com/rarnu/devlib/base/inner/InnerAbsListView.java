package com.rarnu.devlib.base.inner;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.TransitionDrawable;
import android.os.Debug;
import android.os.Parcelable;
import android.util.AttributeSet;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.*;
import android.widget.ListAdapter;
import android.widget.Scroller;
import com.rarnu.devlib.R;

import java.util.ArrayList;
import java.util.List;

public abstract class InnerAbsListView extends InnerAdapterView<ListAdapter> implements ViewTreeObserver.OnGlobalLayoutListener, ViewTreeObserver.OnTouchModeChangeListener {

    public static final int TRANSCRIPT_MODE_DISABLED = 0;
    public static final int TRANSCRIPT_MODE_NORMAL = 1;
    public static final int TRANSCRIPT_MODE_ALWAYS_SCROLL = 2;
    protected static final int TOUCH_MODE_DOWN = 0;
    protected static final int TOUCH_MODE_TAP = 1;
    protected static final int TOUCH_MODE_DONE_WAITING = 2;
    protected static final int TOUCH_MODE_SCROLL = 3;
    protected static final int TOUCH_MODE_FLING = 4;
    static final int TOUCH_MODE_REST = -1;
    static final int LAYOUT_NORMAL = 0;
    static final int LAYOUT_FORCE_TOP = 1;
    static final int LAYOUT_SET_SELECTION = 2;
    static final int LAYOUT_FORCE_BOTTOM = 3;
    static final int LAYOUT_SPECIFIC = 4;
    static final int LAYOUT_SYNC = 5;
    static final int LAYOUT_MOVE_SELECTION = 6;
    private static final int TOUCH_MODE_UNKNOWN = -1;
    private static final int TOUCH_MODE_ON = 0;
    private static final int TOUCH_MODE_OFF = 1;
    private static final boolean PROFILE_SCROLLING = false;
    private static final boolean PROFILE_FLINGING = false;
    private static final int INVALID_POINTER = -1;
    final RecycleBin mRecycler = new RecycleBin();
    final boolean[] mIsScrap = new boolean[1];
    protected ListAdapter mAdapter;
    protected Rect mListPadding = new Rect();
    protected int mWidthMeasureSpec = 0;
    protected boolean mCachingStarted;
    protected int mMotionPosition;
    protected int mTouchMode = TOUCH_MODE_REST;
    int mLayoutMode = LAYOUT_NORMAL;
    AdapterDataSetObserver mDataSetObserver;
    boolean mDrawSelectorOnTop = false;
    Drawable mSelector;
    Rect mSelectorRect = new Rect();
    int mSelectionLeftPadding = 0;
    int mSelectionTopPadding = 0;
    int mSelectionRightPadding = 0;
    int mSelectionBottomPadding = 0;
    int mMotionViewOriginalTop;
    int mMotionViewNewTop;
    int mMotionX;
    int mMotionY;
    int mLastY;
    int mMotionCorrection;
    int mSelectedTop = 0;
    boolean mStackFromBottom;
    boolean mScrollingCacheEnabled;
    int mResurrectToPosition = INVALID_POSITION;
    private VelocityTracker mVelocityTracker;
    private FlingRunnable mFlingRunnable;
    private PositionScroller mPositionScroller;
    private OnScrollListener mOnScrollListener;
    private boolean mSmoothScrollbarEnabled = true;
    private Rect mTouchFrame;
    private ContextMenuInfo mContextMenuInfo = null;
    private int mLastTouchMode = TOUCH_MODE_UNKNOWN;
    private boolean mScrollProfilingStarted = false;
    private boolean mFlingProfilingStarted = false;
    private Runnable mPendingCheckForTap;
    private InnerAbsListView.PerformClick mPerformClick;
    private int mTranscriptMode;
    private int mCacheColorHint;
    private boolean mIsChildViewEnabled;
    private int mLastScrollState = OnScrollListener.SCROLL_STATE_IDLE;
    private int mTouchSlop;
    private Runnable mClearScrollingCache;
    private int mMinimumVelocity;
    private int mMaximumVelocity;
    private int mActivePointerId = INVALID_POINTER;

    public InnerAbsListView(Context context) {
        super(context);
        initAbsListView();

        setVerticalScrollBarEnabled(true);
        TypedArray a = context.obtainStyledAttributes(R.styleable.View);
        initializeScrollbars(a);
        a.recycle();
    }

    public InnerAbsListView(Context context, AttributeSet attrs) {
        this(context, attrs, R.attr.absListViewStyle);
    }

    public InnerAbsListView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        initAbsListView();

        TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.AbsListView, defStyle, 0);

        Drawable d = a.getDrawable(R.styleable.AbsListView_listSelector);
        if (d != null) {
            setSelector(d);
        }

        mDrawSelectorOnTop = a.getBoolean(R.styleable.AbsListView_drawSelectorOnTop, false);

        boolean stackFromBottom = a.getBoolean(R.styleable.AbsListView_stackFromBottom, false);
        setStackFromBottom(stackFromBottom);

        boolean scrollingCacheEnabled = a.getBoolean(R.styleable.AbsListView_scrollingCache, true);
        setScrollingCacheEnabled(scrollingCacheEnabled);

        int transcriptMode = a.getInt(R.styleable.AbsListView_transcriptMode, TRANSCRIPT_MODE_DISABLED);
        setTranscriptMode(transcriptMode);

        int color = a.getColor(R.styleable.AbsListView_cacheColorHint, 0);
        setCacheColorHint(color);

        boolean smoothScrollbar = a.getBoolean(R.styleable.AbsListView_smoothScrollbar, true);
        setSmoothScrollbarEnabled(smoothScrollbar);

        a.recycle();
    }

    static int getDistance(Rect source, Rect dest, int direction) {
        int sX, sY;
        int dX, dY;
        switch (direction) {
            case View.FOCUS_RIGHT:
                sX = source.right;
                sY = source.top + source.height() / 2;
                dX = dest.left;
                dY = dest.top + dest.height() / 2;
                break;
            case View.FOCUS_DOWN:
                sX = source.left + source.width() / 2;
                sY = source.bottom;
                dX = dest.left + dest.width() / 2;
                dY = dest.top;
                break;
            case View.FOCUS_LEFT:
                sX = source.left;
                sY = source.top + source.height() / 2;
                dX = dest.right;
                dY = dest.top + dest.height() / 2;
                break;
            case View.FOCUS_UP:
                sX = source.left + source.width() / 2;
                sY = source.top;
                dX = dest.left + dest.width() / 2;
                dY = dest.bottom;
                break;
            default:
                throw new IllegalArgumentException("direction must be one of {FOCUS_UP, FOCUS_DOWN, FOCUS_LEFT, FOCUS_RIGHT}.");
        }
        int deltaX = dX - sX;
        int deltaY = dY - sY;
        return deltaY * deltaY + deltaX * deltaX;
    }

    private void initAbsListView() {
        setClickable(true);
        setFocusableInTouchMode(true);
        setWillNotDraw(false);
        setAlwaysDrawnWithCacheEnabled(false);
        setScrollingCacheEnabled(true);

        final ViewConfiguration configuration = ViewConfiguration.get(getContext());
        mTouchSlop = configuration.getScaledTouchSlop();
        mMinimumVelocity = configuration.getScaledMinimumFlingVelocity();
        mMaximumVelocity = configuration.getScaledMaximumFlingVelocity();
    }

    public boolean isSmoothScrollbarEnabled() {
        return mSmoothScrollbarEnabled;
    }

    public void setSmoothScrollbarEnabled(boolean enabled) {
        mSmoothScrollbarEnabled = enabled;
    }

    public void setOnScrollListener(OnScrollListener l) {
        mOnScrollListener = l;
        invokeOnItemScrollListener();
    }

    void invokeOnItemScrollListener() {
        if (mOnScrollListener != null) {
            mOnScrollListener.onScroll(this, mFirstPosition, getChildCount(), mItemCount);
        }
    }

    public boolean isScrollingCacheEnabled() {
        return mScrollingCacheEnabled;
    }

    public void setScrollingCacheEnabled(boolean enabled) {
        if (mScrollingCacheEnabled && !enabled) {
            clearScrollingCache();
        }
        mScrollingCacheEnabled = enabled;
    }

    @Override
    public void getFocusedRect(Rect r) {
        View view = getSelectedView();
        if (view != null && view.getParent() == this) {
            view.getFocusedRect(r);
            offsetDescendantRectToMyCoords(view, r);
        } else {
            super.getFocusedRect(r);
        }
    }

    private void useDefaultSelector() {
        setSelector(getResources().getDrawable(android.R.drawable.list_selector_background));
    }

    public boolean isStackFromBottom() {
        return mStackFromBottom;
    }

    public void setStackFromBottom(boolean stackFromBottom) {
        if (mStackFromBottom != stackFromBottom) {
            mStackFromBottom = stackFromBottom;
            requestLayoutIfNecessary();
        }
    }

    void requestLayoutIfNecessary() {
        if (getChildCount() > 0) {
            resetList();
            requestLayout();
            invalidate();
        }
    }

    @Override
    public void onRestoreInstanceState(Parcelable state) {
        super.onRestoreInstanceState(state);
        mDataChanged = true;
        requestLayout();
    }

    @Override
    public void requestLayout() {
        if (!mBlockLayoutRequests && !mInLayout) {
            super.requestLayout();
        }
    }

    void resetList() {
        removeAllViewsInLayout();
        mFirstPosition = 0;
        mDataChanged = false;
        mNeedSync = false;
        mOldSelectedPosition = INVALID_POSITION;
        mOldSelectedRowId = INVALID_ROW_ID;
        mSelectedTop = 0;
        mSelectorRect.setEmpty();
        invalidate();
    }

    @Override
    protected int computeVerticalScrollExtent() {
        final int count = getChildCount();
        if (count > 0) {
            if (mSmoothScrollbarEnabled) {
                int extent = count * 100;

                View view = getChildAt(0);
                final int top = getFillChildTop();

                int height = view.getHeight();
                if (height > 0) {
                    extent += (top * 100) / height;
                }

                view = getChildAt(count - 1);
                final int bottom = getScrollChildBottom();
                height = view.getHeight();
                if (height > 0) {
                    extent -= ((bottom - getHeight()) * 100) / height;
                }

                return extent;
            } else {
                return 1;
            }
        }
        return 0;
    }

    @Override
    protected int computeVerticalScrollOffset() {
        final int firstPosition = mFirstPosition;
        final int childCount = getChildCount();
        if (firstPosition >= 0 && childCount > 0) {
            if (mSmoothScrollbarEnabled) {
                final View view = getChildAt(0);
                final int top = getFillChildTop();
                int height = view.getHeight();
                if (height > 0) {
                    return Math.max(firstPosition * 100 - (top * 100) / height + (int) ((float) getScrollY() / getHeight() * mItemCount * 100), 0);
                }
            } else {
                int index;
                final int count = mItemCount;
                if (firstPosition == 0) {
                    index = 0;
                } else if (firstPosition + childCount == count) {
                    index = count;
                } else {
                    index = firstPosition + childCount / 2;
                }
                return (int) (firstPosition + childCount * (index / (float) count));
            }
        }
        return 0;
    }

    @Override
    protected int computeVerticalScrollRange() {
        int result;
        if (mSmoothScrollbarEnabled) {
            result = Math.max(mItemCount * 100, 0);
        } else {
            result = mItemCount;
        }
        return result;
    }

    @Override
    protected float getTopFadingEdgeStrength() {
        final int count = getChildCount();
        final float fadeEdge = super.getTopFadingEdgeStrength();
        if (count == 0) {
            return fadeEdge;
        } else {
            if (mFirstPosition > 0) {
                return 1.0f;
            }

            final int top = getChildAt(0).getTop();
            final float fadeLength = (float) getVerticalFadingEdgeLength();
            return top < getPaddingTop() ? (float) -(top - getPaddingTop()) / fadeLength : fadeEdge;
        }
    }

    @Override
    protected float getBottomFadingEdgeStrength() {
        final int count = getChildCount();
        final float fadeEdge = super.getBottomFadingEdgeStrength();
        if (count == 0) {
            return fadeEdge;
        } else {
            if (mFirstPosition + count - 1 < mItemCount - 1) {
                return 1.0f;
            }

            final int bottom = getChildAt(count - 1).getBottom();
            final int height = getHeight();
            final float fadeLength = (float) getVerticalFadingEdgeLength();
            return bottom > height - getPaddingBottom() ? (float) (bottom - height + getPaddingBottom()) / fadeLength : fadeEdge;
        }
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        if (mSelector == null) {
            useDefaultSelector();
        }
        final Rect listPadding = mListPadding;
        listPadding.left = mSelectionLeftPadding + getPaddingLeft();
        listPadding.top = mSelectionTopPadding + getPaddingTop();
        listPadding.right = mSelectionRightPadding + getPaddingRight();
        listPadding.bottom = mSelectionBottomPadding + getPaddingBottom();
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        super.onLayout(changed, l, t, r, b);
        mInLayout = true;
        if (changed) {
            int childCount = getChildCount();
            for (int i = 0; i < childCount; i++) {
                getChildAt(i).forceLayout();
            }
            mRecycler.markChildrenDirty();
        }
        layoutChildren();

        mInLayout = false;
    }

    protected void layoutChildren() {
    }

    @Override
    public View getSelectedView() {
        return null;
    }

    public int getListPaddingTop() {
        return mListPadding.top;
    }

    public int getListPaddingBottom() {
        return mListPadding.bottom;
    }

    public int getListPaddingLeft() {
        return mListPadding.left;
    }

    public int getListPaddingRight() {
        return mListPadding.right;
    }

    View obtainView(int position, boolean[] isScrap) {
        isScrap[0] = false;
        View scrapView;

        scrapView = mRecycler.getScrapView(position);

        View child;
        if (scrapView != null) {
            if (ViewDebug.TRACE_RECYCLER) {
                ViewDebug.trace(scrapView, ViewDebug.RecyclerTraceType.RECYCLE_FROM_SCRAP_HEAP, position, -1);
            }

            child = mAdapter.getView(position, scrapView, this);

            if (ViewDebug.TRACE_RECYCLER) {
                ViewDebug.trace(child, ViewDebug.RecyclerTraceType.BIND_VIEW, position, getChildCount());
            }

            if (child != scrapView) {
                mRecycler.addScrapView(scrapView);
                if (mCacheColorHint != 0) {
                    child.setDrawingCacheBackgroundColor(mCacheColorHint);
                }
                if (ViewDebug.TRACE_RECYCLER) {
                    ViewDebug.trace(scrapView, ViewDebug.RecyclerTraceType.MOVE_TO_SCRAP_HEAP, position, -1);
                }
            } else {
                isScrap[0] = true;
                dispatchFinishTemporaryDetach(child);
            }
        } else {
            child = mAdapter.getView(position, null, this);
            if (mCacheColorHint != 0) {
                child.setDrawingCacheBackgroundColor(mCacheColorHint);
            }
            if (ViewDebug.TRACE_RECYCLER) {
                ViewDebug.trace(child, ViewDebug.RecyclerTraceType.NEW_VIEW, position, getChildCount());
            }
        }

        return child;
    }

    void positionSelector(View sel) {
        final Rect selectorRect = mSelectorRect;
        selectorRect.set(sel.getLeft(), sel.getTop(), sel.getRight(), sel.getBottom());
        positionSelector(selectorRect.left, selectorRect.top, selectorRect.right, selectorRect.bottom);

        final boolean isChildViewEnabled = mIsChildViewEnabled;
        if (sel.isEnabled() != isChildViewEnabled) {
            mIsChildViewEnabled = !isChildViewEnabled;
            refreshDrawableState();
        }
    }

    private void positionSelector(int l, int t, int r, int b) {
        mSelectorRect.set(l - mSelectionLeftPadding, t - mSelectionTopPadding, r + mSelectionRightPadding, b + mSelectionBottomPadding);
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {

        final boolean drawSelectorOnTop = mDrawSelectorOnTop;
        if (!drawSelectorOnTop) {
            drawSelector(canvas);
        }

        super.dispatchDraw(canvas);

        if (drawSelectorOnTop) {
            drawSelector(canvas);
        }
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        if (getChildCount() > 0) {
            mDataChanged = true;
            rememberSyncState();
        }
    }

    boolean touchModeDrawsInPressedState() {
        switch (mTouchMode) {
            case TOUCH_MODE_TAP:
            case TOUCH_MODE_DONE_WAITING:
                return true;
            default:
                return false;
        }
    }

    protected boolean shouldShowSelector() {
        return (hasFocus() && !isInTouchMode()) || touchModeDrawsInPressedState();
    }

    private void drawSelector(Canvas canvas) {
        if (shouldShowSelector() && mSelectorRect != null && !mSelectorRect.isEmpty()) {
            final Drawable selector = mSelector;
            selector.setBounds(mSelectorRect);
            selector.draw(canvas);
        }
    }

    public void setDrawSelectorOnTop(boolean onTop) {
        mDrawSelectorOnTop = onTop;
    }

    public void setSelector(int resID) {
        setSelector(getResources().getDrawable(resID));
    }

    public Drawable getSelector() {
        return mSelector;
    }

    public void setSelector(Drawable sel) {
        if (mSelector != null) {
            mSelector.setCallback(null);
            unscheduleDrawable(mSelector);
        }
        mSelector = sel;
        Rect padding = new Rect();
        sel.getPadding(padding);
        mSelectionLeftPadding = padding.left;
        mSelectionTopPadding = padding.top;
        mSelectionRightPadding = padding.right;
        mSelectionBottomPadding = padding.bottom;
        sel.setCallback(this);
        sel.setState(getDrawableState());
    }

    @Override
    protected void drawableStateChanged() {
        super.drawableStateChanged();
        if (mSelector != null) {
            mSelector.setState(getDrawableState());
        }
    }

    @Override
    protected int[] onCreateDrawableState(int extraSpace) {
        if (mIsChildViewEnabled) {
            return super.onCreateDrawableState(extraSpace);
        }

        final int enabledState = ENABLED_STATE_SET[0];

        int[] state = super.onCreateDrawableState(extraSpace + 1);
        int enabledPos = -1;
        for (int i = state.length - 1; i >= 0; i--) {
            if (state[i] == enabledState) {
                enabledPos = i;
                break;
            }
        }

        if (enabledPos >= 0) {
            System.arraycopy(state, enabledPos + 1, state, enabledPos, state.length - enabledPos - 1);
        }

        return state;
    }

    @Override
    public boolean verifyDrawable(Drawable dr) {
        return mSelector == dr || super.verifyDrawable(dr);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();

        final ViewTreeObserver treeObserver = getViewTreeObserver();
        if (treeObserver != null) {
            treeObserver.addOnTouchModeChangeListener(this);
        }
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mRecycler.clear();

        final ViewTreeObserver treeObserver = getViewTreeObserver();
        if (treeObserver != null) {
            treeObserver.removeOnTouchModeChangeListener(this);
        }
    }

    @Override
    public void onWindowFocusChanged(boolean hasWindowFocus) {
        super.onWindowFocusChanged(hasWindowFocus);

        final int touchMode = isInTouchMode() ? TOUCH_MODE_ON : TOUCH_MODE_OFF;

        if (!hasWindowFocus) {
            setChildrenDrawingCacheEnabled(false);
            if (mFlingRunnable != null) {
                removeCallbacks(mFlingRunnable);
                mFlingRunnable.endFling();

                if (getScrollY() != 0) {
                    scrollTo(getScrollX(), 0);
                    invalidate();
                }
            }
        } else {

            if (touchMode != mLastTouchMode && mLastTouchMode != TOUCH_MODE_UNKNOWN) {
                mLayoutMode = LAYOUT_NORMAL;
                layoutChildren();
            }
        }

        mLastTouchMode = touchMode;
    }

    ContextMenuInfo createContextMenuInfo(View view, int position, long id) {
        return new AdapterContextMenuInfo(view, position, id);
    }

    @Override
    protected ContextMenuInfo getContextMenuInfo() {
        return mContextMenuInfo;
    }

    @Override
    public boolean showContextMenuForChild(View originalView) {
        final int longPressPosition = getPositionForView(originalView);
        if (longPressPosition >= 0) {
            final long longPressId = mAdapter.getItemId(longPressPosition);
            boolean handled = false;

            if (mOnItemLongClickListener != null) {
                handled = mOnItemLongClickListener.onItemLongClick(InnerAbsListView.this, originalView, longPressPosition, longPressId);
            }
            if (!handled) {
                mContextMenuInfo = createContextMenuInfo(getChildAt(longPressPosition - mFirstPosition), longPressPosition, longPressId);
                handled = super.showContextMenuForChild(originalView);
            }

            return handled;
        }
        return false;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        return false;
    }

    @Override
    protected void dispatchSetPressed(boolean pressed) {

    }

    public int pointToPosition(int x, int y) {
        Rect frame = mTouchFrame;
        if (frame == null) {
            mTouchFrame = new Rect();
            frame = mTouchFrame;
        }

        final int count = getChildCount();
        for (int i = count - 1; i >= 0; i--) {
            final View child = getChildAt(i);
            if (child.getVisibility() == View.VISIBLE) {
                child.getHitRect(frame);
                if (frame.contains(x, y)) {
                    return mFirstPosition + i;
                }
            }
        }
        return INVALID_POSITION;
    }

    public long pointToRowId(int x, int y) {
        int position = pointToPosition(x, y);
        if (position >= 0) {
            return mAdapter.getItemId(position);
        }
        return INVALID_ROW_ID;
    }

    private boolean startScrollIfNeeded(int deltaY) {
        final int distance = Math.abs(deltaY);
        if (distance > mTouchSlop) {
            createScrollingCache();
            mTouchMode = TOUCH_MODE_SCROLL;
            mMotionCorrection = deltaY;
            setPressed(false);
            View motionView = getChildAt(mMotionPosition - mFirstPosition);
            if (motionView != null) {
                motionView.setPressed(false);
            }
            reportScrollStateChange(OnScrollListener.SCROLL_STATE_TOUCH_SCROLL);
            requestDisallowInterceptTouchEvent(true);
            return true;
        }

        return false;
    }

    public void onTouchModeChanged(boolean isInTouchMode) {
        if (isInTouchMode) {
            if (getHeight() > 0 && getChildCount() > 0) {
                layoutChildren();
            }
        }
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        if (!isEnabled()) {
            return isClickable() || isLongClickable();
        }

        final int action = ev.getAction();

        View v;
        int deltaY;

        if (mVelocityTracker == null) {
            mVelocityTracker = VelocityTracker.obtain();
        }
        mVelocityTracker.addMovement(ev);

        switch (action & MotionEvent.ACTION_MASK) {
            case MotionEvent.ACTION_DOWN: {
                mActivePointerId = ev.getPointerId(0);
                final int x = (int) ev.getX();
                final int y = (int) ev.getY();
                int motionPosition = pointToPosition(x, y);
                if (!mDataChanged) {
                    if ((mTouchMode != TOUCH_MODE_FLING) && (motionPosition >= 0) && (getAdapter().isEnabled(motionPosition))) {
                        mTouchMode = TOUCH_MODE_DOWN;
                        if (mPendingCheckForTap == null) {
                            mPendingCheckForTap = new CheckForTap();
                        }
                        postDelayed(mPendingCheckForTap, ViewConfiguration.getTapTimeout());
                    } else {
                        if (ev.getEdgeFlags() != 0 && motionPosition < 0) {
                            return false;
                        }

                        if (mTouchMode == TOUCH_MODE_FLING) {
                            createScrollingCache();
                            mTouchMode = TOUCH_MODE_SCROLL;
                            mMotionCorrection = 0;
                            motionPosition = findMotionRow(y);
                            reportScrollStateChange(OnScrollListener.SCROLL_STATE_TOUCH_SCROLL);
                        }
                    }
                }

                if (motionPosition >= 0) {
                    v = getChildAt(motionPosition - mFirstPosition);
                    mMotionViewOriginalTop = v.getTop();
                }
                mMotionX = x;
                mMotionY = y;
                mMotionPosition = motionPosition;
                mLastY = Integer.MIN_VALUE;
                break;
            }

            case MotionEvent.ACTION_MOVE: {
                final int pointerIndex = ev.findPointerIndex(mActivePointerId);
                final int y = (int) ev.getY(pointerIndex);
                deltaY = y - mMotionY;
                switch (mTouchMode) {
                    case TOUCH_MODE_DOWN:
                    case TOUCH_MODE_TAP:
                    case TOUCH_MODE_DONE_WAITING:
                        startScrollIfNeeded(deltaY);
                        break;
                    case TOUCH_MODE_SCROLL:
                        if (PROFILE_SCROLLING) {
                            if (!mScrollProfilingStarted) {
                                Debug.startMethodTracing("AbsListViewScroll");
                                mScrollProfilingStarted = true;
                            }
                        }

                        if (y != mLastY) {
                            deltaY -= mMotionCorrection;
                            int incrementalDeltaY = mLastY != Integer.MIN_VALUE ? y - mLastY : deltaY;
                            boolean atEdge = false;
                            if (incrementalDeltaY != 0) {
                                atEdge = trackMotionScroll(deltaY, incrementalDeltaY);
                            }

                            if (atEdge && getChildCount() > 0) {
                                int motionPosition = findMotionRow(y);
                                if (motionPosition >= 0) {
                                    final View motionView = getChildAt(motionPosition - mFirstPosition);
                                    mMotionViewOriginalTop = motionView.getTop();
                                }
                                mMotionY = y;
                                mMotionPosition = motionPosition;
                                invalidate();
                            }
                            mLastY = y;
                        }
                        break;
                }

                break;
            }

            case MotionEvent.ACTION_UP: {
                switch (mTouchMode) {
                    case TOUCH_MODE_DOWN:
                    case TOUCH_MODE_TAP:
                    case TOUCH_MODE_DONE_WAITING:
                        final int motionPosition = mMotionPosition;
                        final View child = getChildAt(motionPosition - mFirstPosition);
                        if (child != null && !child.hasFocusable()) {
                            if (mTouchMode != TOUCH_MODE_DOWN) {
                                child.setPressed(false);
                            }

                            if (mPerformClick == null) {
                                mPerformClick = new PerformClick();
                            }

                            final InnerAbsListView.PerformClick performClick = mPerformClick;
                            performClick.mChild = child;
                            performClick.mClickMotionPosition = motionPosition;
                            performClick.rememberWindowAttachCount();

                            mResurrectToPosition = motionPosition;

                            if (mTouchMode == TOUCH_MODE_DOWN || mTouchMode == TOUCH_MODE_TAP) {
                                mLayoutMode = LAYOUT_NORMAL;
                                if (!mDataChanged && mAdapter.isEnabled(motionPosition)) {
                                    mTouchMode = TOUCH_MODE_TAP;
                                    layoutChildren();
                                    child.setPressed(true);
                                    positionSelector(child);
                                    setPressed(true);
                                    if (mSelector != null) {
                                        Drawable d = mSelector.getCurrent();
                                        if (d != null && d instanceof TransitionDrawable) {
                                            ((TransitionDrawable) d).resetTransition();
                                        }
                                    }
                                    postDelayed(new Runnable() {
                                        public void run() {
                                            child.setPressed(false);
                                            setPressed(false);
                                            if (!mDataChanged) {
                                                post(performClick);
                                            }
                                            mTouchMode = TOUCH_MODE_REST;
                                        }
                                    }, ViewConfiguration.getPressedStateDuration());
                                } else {
                                    mTouchMode = TOUCH_MODE_REST;
                                }
                                return true;
                            } else if (!mDataChanged && mAdapter.isEnabled(motionPosition)) {
                                post(performClick);
                            }
                        }
                        mTouchMode = TOUCH_MODE_REST;
                        break;
                    case TOUCH_MODE_SCROLL:
                        final int childCount = getChildCount();
                        if (childCount > 0) {
                            int top = getFillChildTop();
                            int bottom = getFillChildBottom();
                            if (mFirstPosition == 0 && top >= mListPadding.top && mFirstPosition + childCount < mItemCount && bottom <= getHeight() - mListPadding.bottom) {
                                mTouchMode = TOUCH_MODE_REST;
                                reportScrollStateChange(OnScrollListener.SCROLL_STATE_IDLE);
                            } else {
                                final VelocityTracker velocityTracker = mVelocityTracker;
                                velocityTracker.computeCurrentVelocity(1000, mMaximumVelocity);
                                final int initialVelocity = (int) velocityTracker.getYVelocity(mActivePointerId);

                                if (Math.abs(initialVelocity) > mMinimumVelocity) {
                                    if (mFlingRunnable == null) {
                                        mFlingRunnable = new FlingRunnable();
                                    }
                                    reportScrollStateChange(OnScrollListener.SCROLL_STATE_FLING);

                                    mFlingRunnable.start(-initialVelocity);
                                } else {
                                    mTouchMode = TOUCH_MODE_REST;
                                    reportScrollStateChange(OnScrollListener.SCROLL_STATE_IDLE);
                                }
                            }
                        } else {
                            mTouchMode = TOUCH_MODE_REST;
                            reportScrollStateChange(OnScrollListener.SCROLL_STATE_IDLE);
                        }
                        break;
                }

                setPressed(false);
                invalidate();

                if (mVelocityTracker != null) {
                    mVelocityTracker.recycle();
                    mVelocityTracker = null;
                }

                mActivePointerId = INVALID_POINTER;

                if (PROFILE_SCROLLING) {
                    if (mScrollProfilingStarted) {
                        Debug.stopMethodTracing();
                        mScrollProfilingStarted = false;
                    }
                }
                break;
            }

            case MotionEvent.ACTION_CANCEL: {
                mTouchMode = TOUCH_MODE_REST;
                setPressed(false);
                View motionView = this.getChildAt(mMotionPosition - mFirstPosition);
                if (motionView != null) {
                    motionView.setPressed(false);
                }
                clearScrollingCache();

                if (mVelocityTracker != null) {
                    mVelocityTracker.recycle();
                    mVelocityTracker = null;
                }

                mActivePointerId = INVALID_POINTER;
                break;
            }

            case MotionEvent.ACTION_POINTER_UP: {
                onSecondaryPointerUp(ev);
                final int x = mMotionX;
                final int y = mMotionY;
                final int motionPosition = pointToPosition(x, y);
                if (motionPosition >= 0) {
                    v = getChildAt(motionPosition - mFirstPosition);
                    mMotionViewOriginalTop = v.getTop();
                    mMotionPosition = motionPosition;
                }
                mLastY = y;
                break;
            }
        }

        return true;
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {
        int action = ev.getAction();
        View v;

        switch (action & MotionEvent.ACTION_MASK) {
            case MotionEvent.ACTION_DOWN: {
                int touchMode = mTouchMode;

                final int x = (int) ev.getX();
                final int y = (int) ev.getY();
                mActivePointerId = ev.getPointerId(0);

                int motionPosition = findMotionRow(y);
                if (touchMode != TOUCH_MODE_FLING && motionPosition >= 0) {
                    v = getChildAt(motionPosition - mFirstPosition);
                    mMotionViewOriginalTop = v.getTop();
                    mMotionX = x;
                    mMotionY = y;
                    mMotionPosition = motionPosition;
                    mTouchMode = TOUCH_MODE_DOWN;
                    clearScrollingCache();
                }
                mLastY = Integer.MIN_VALUE;
                if (touchMode == TOUCH_MODE_FLING) {
                    return true;
                }
                break;
            }

            case MotionEvent.ACTION_MOVE: {
                switch (mTouchMode) {
                    case TOUCH_MODE_DOWN:
                        final int pointerIndex = ev.findPointerIndex(mActivePointerId);
                        final int y = (int) ev.getY(pointerIndex);
                        if (startScrollIfNeeded(y - mMotionY)) {
                            return true;
                        }
                        break;
                }
                break;
            }

            case MotionEvent.ACTION_UP: {
                mTouchMode = TOUCH_MODE_REST;
                mActivePointerId = INVALID_POINTER;
                reportScrollStateChange(OnScrollListener.SCROLL_STATE_IDLE);
                break;
            }

            case MotionEvent.ACTION_POINTER_UP: {
                onSecondaryPointerUp(ev);
                break;
            }
        }

        return false;
    }

    private void onSecondaryPointerUp(MotionEvent ev) {
        final int pointerIndex = (ev.getAction() & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT;
        final int pointerId = ev.getPointerId(pointerIndex);
        if (pointerId == mActivePointerId) {
            final int newPointerIndex = pointerIndex == 0 ? 1 : 0;
            mMotionX = (int) ev.getX(newPointerIndex);
            mMotionY = (int) ev.getY(newPointerIndex);
            mActivePointerId = ev.getPointerId(newPointerIndex);
            if (mVelocityTracker != null) {
                mVelocityTracker.clear();
            }
        }
    }

    @Override
    public void addTouchables(ArrayList<View> views) {
        final int count = getChildCount();
        final int firstPosition = mFirstPosition;
        final ListAdapter adapter = mAdapter;

        if (adapter == null) {
            return;
        }

        for (int i = 0; i < count; i++) {
            final View child = getChildAt(i);
            if (adapter.isEnabled(firstPosition + i)) {
                views.add(child);
            }
            child.addTouchables(views);
        }
    }

    void reportScrollStateChange(int newState) {
        if (newState != mLastScrollState) {
            if (mOnScrollListener != null) {
                mOnScrollListener.onScrollStateChanged(this, newState);
                mLastScrollState = newState;
            }
        }
    }

    public void smoothScrollToPosition(int position) {
        if (mPositionScroller == null) {
            mPositionScroller = new PositionScroller();
        }
        mPositionScroller.start(position);
    }

    public void smoothScrollToPosition(int position, int boundPosition) {
        if (mPositionScroller == null) {
            mPositionScroller = new PositionScroller();
        }
        mPositionScroller.start(position, boundPosition);
    }

    public void smoothScrollBy(int distance, int duration) {
        if (mFlingRunnable == null) {
            mFlingRunnable = new FlingRunnable();
        } else {
            mFlingRunnable.endFling();
        }
        mFlingRunnable.startScroll(distance, duration);
    }

    private void createScrollingCache() {
        if (mScrollingCacheEnabled && !mCachingStarted) {
            setChildrenDrawnWithCacheEnabled(true);
            setChildrenDrawingCacheEnabled(true);
            mCachingStarted = true;
        }
    }

    private void clearScrollingCache() {
        if (mClearScrollingCache == null) {
            mClearScrollingCache = new Runnable() {
                public void run() {
                    if (mCachingStarted) {
                        mCachingStarted = false;
                        setChildrenDrawnWithCacheEnabled(false);
                        final int mPersistentDrawingCache = getPersistentDrawingCache();
                        if ((mPersistentDrawingCache & PERSISTENT_SCROLLING_CACHE) == 0) {
                            setChildrenDrawingCacheEnabled(false);
                        }
                        if (!isAlwaysDrawnWithCacheEnabled()) {
                            invalidate();
                        }
                    }
                }
            };
        }
        post(mClearScrollingCache);
    }

    boolean trackMotionScroll(int deltaY, int incrementalDeltaY) {
        final int childCount = getChildCount();
        if (childCount == 0) {
            return true;
        }

        final int firstTop = getScrollChildTop();
        final int lastBottom = getScrollChildBottom();
        final Rect listPadding = mListPadding;

        final int end = getHeight() - listPadding.bottom;
        final int spaceAbove = listPadding.top - getFillChildTop();
        final int spaceBelow = getFillChildBottom() - end;
        final int height = getHeight() - getPaddingBottom() - getPaddingTop();
        if (deltaY < 0) {
            deltaY = Math.max(-(height - 1), deltaY);
        } else {
            deltaY = Math.min(height - 1, deltaY);
        }

        if (incrementalDeltaY < 0) {
            incrementalDeltaY = Math.max(-(height - 1), incrementalDeltaY);
        } else {
            incrementalDeltaY = Math.min(height - 1, incrementalDeltaY);
        }

        final int firstPosition = mFirstPosition;

        if (firstPosition == 0 && firstTop >= listPadding.top && deltaY >= 0) {
            return true;
        }

        if (firstPosition + childCount == mItemCount && lastBottom <= end
                && deltaY <= 0) {
            return true;
        }

        final boolean down = incrementalDeltaY < 0;

        final int headerViewsCount = getHeaderViewsCount();
        final int footerViewsStart = mItemCount - getFooterViewsCount();

        int start = 0;
        int count = 0;

        if (down) {
            final int top = listPadding.top - incrementalDeltaY;
            for (int i = 0; i < childCount; i++) {
                final View child = getChildAt(i);
                if (child.getBottom() >= top) {
                    break;
                } else {
                    count++;
                    int position = firstPosition + i;
                    if (position >= headerViewsCount && position < footerViewsStart) {
                        mRecycler.addScrapView(child);

                        if (ViewDebug.TRACE_RECYCLER) {
                            ViewDebug.trace(child, ViewDebug.RecyclerTraceType.MOVE_TO_SCRAP_HEAP, firstPosition + i, -1);
                        }
                    }
                }
            }
        } else {
            final int bottom = getHeight() - listPadding.bottom - incrementalDeltaY;
            for (int i = childCount - 1; i >= 0; i--) {
                final View child = getChildAt(i);
                if (child.getTop() <= bottom) {
                    break;
                } else {
                    start = i;
                    count++;
                    int position = firstPosition + i;
                    if (position >= headerViewsCount && position < footerViewsStart) {
                        mRecycler.addScrapView(child);

                        if (ViewDebug.TRACE_RECYCLER) {
                            ViewDebug.trace(child, ViewDebug.RecyclerTraceType.MOVE_TO_SCRAP_HEAP, firstPosition + i, -1);
                        }
                    }
                }
            }
        }

        mMotionViewNewTop = mMotionViewOriginalTop + deltaY;

        mBlockLayoutRequests = true;

        if (count > 0) {
            detachViewsFromParent(start, count);
        }

        tryOffsetChildrenTopAndBottom(incrementalDeltaY);

        if (down) {
            mFirstPosition += count;
        }

        invalidate();

        final int absIncrementalDeltaY = Math.abs(incrementalDeltaY);
        if (spaceAbove < absIncrementalDeltaY || spaceBelow < absIncrementalDeltaY) {
            fillGap(down);
        }

        mBlockLayoutRequests = false;
        invokeOnItemScrollListener();
        awakenScrollBars();

        return false;
    }

    protected void tryOffsetChildrenTopAndBottom(int offset) {
        final int count = getChildCount();

        for (int i = 0; i < count; i++) {
            final View v = getChildAt(i);
            v.offsetTopAndBottom(offset);
        }
    }

    int getHeaderViewsCount() {
        return 0;
    }

    int getFooterViewsCount() {
        return 0;
    }

    abstract void fillGap(boolean down);

    abstract int findMotionRow(int y);

    int findClosestMotionRow(int y) {
        final int childCount = getChildCount();
        if (childCount == 0) {
            return INVALID_POSITION;
        }

        final int motionRow = findMotionRow(y);
        return motionRow != INVALID_POSITION ? motionRow : mFirstPosition + childCount - 1;
    }

    public void invalidateViews() {
        mDataChanged = true;
        rememberSyncState();
        requestLayout();
        invalidate();
    }

    @Override
    protected void handleDataChanged() {
        int count = mItemCount;
        if (count > 0) {

            int newPos;
            int selectablePos;

            if (mNeedSync) {
                mNeedSync = false;
                if (mTranscriptMode == TRANSCRIPT_MODE_ALWAYS_SCROLL || (mTranscriptMode == TRANSCRIPT_MODE_NORMAL && mFirstPosition + getChildCount() >= mOldItemCount)) {
                    mLayoutMode = LAYOUT_FORCE_BOTTOM;
                    return;
                }

                switch (mSyncMode) {
                    case SYNC_FIRST_POSITION:
                        mLayoutMode = LAYOUT_SYNC;
                        mSyncPosition = Math.min(Math.max(0, mSyncPosition), count - 1);
                        return;
                }
            }

            if (!isInTouchMode()) {
                newPos = getSelectedItemPosition();
                if (newPos >= count) {
                    newPos = count - 1;
                }
                if (newPos < 0) {
                    newPos = 0;
                }

                selectablePos = lookForSelectablePosition(newPos, true);

                selectablePos = lookForSelectablePosition(newPos, false);
                if (selectablePos >= 0) {
                    return;
                }
            } else {

                if (mResurrectToPosition >= 0) {
                    return;
                }
            }

        }

        mLayoutMode = mStackFromBottom ? LAYOUT_FORCE_BOTTOM : LAYOUT_FORCE_TOP;
        mNeedSync = false;
    }

    protected void onLayoutSync(int syncPosition) {
    }

    protected void onLayoutSyncFinished(int syncPosition) {
    }

    @Override
    public void onGlobalLayout() {
    }

    @Override
    protected ViewGroup.LayoutParams generateLayoutParams(ViewGroup.LayoutParams p) {
        return new LayoutParams(p);
    }

    @Override
    public LayoutParams generateLayoutParams(AttributeSet attrs) {
        return new InnerAbsListView.LayoutParams(getContext(), attrs);
    }

    @Override
    protected boolean checkLayoutParams(ViewGroup.LayoutParams p) {
        return p instanceof InnerAbsListView.LayoutParams;
    }

    public int getTranscriptMode() {
        return mTranscriptMode;
    }

    public void setTranscriptMode(int mode) {
        mTranscriptMode = mode;
    }

    @Override
    public int getSolidColor() {
        return mCacheColorHint;
    }

    public int getCacheColorHint() {
        return mCacheColorHint;
    }

    public void setCacheColorHint(int color) {
        if (color != mCacheColorHint) {
            mCacheColorHint = color;
            int count = getChildCount();
            for (int i = 0; i < count; i++) {
                getChildAt(i).setDrawingCacheBackgroundColor(color);
            }
            mRecycler.setCacheColorHint(color);
        }
    }

    public void reclaimViews(List<View> views) {
        int childCount = getChildCount();
        RecyclerListener listener = mRecycler.mRecyclerListener;

        for (int i = 0; i < childCount; i++) {
            View child = getChildAt(i);
            InnerAbsListView.LayoutParams lp = (InnerAbsListView.LayoutParams) child.getLayoutParams();

            if (lp != null && mRecycler.shouldRecycleViewType(lp.viewType)) {
                views.add(child);
                if (listener != null) {
                    listener.onMovedToScrapHeap(child);
                }
            }
        }
        mRecycler.reclaimScrapViews(views);
        removeAllViewsInLayout();
    }

    public void setRecyclerListener(RecyclerListener listener) {
        mRecycler.mRecyclerListener = listener;
    }

    private void dispatchFinishTemporaryDetach(View v) {
        if (v == null) {
            return;
        }

        v.onFinishTemporaryDetach();
        if (v instanceof ViewGroup) {
            ViewGroup group = (ViewGroup) v;
            final int count = group.getChildCount();
            for (int i = 0; i < count; i++) {
                dispatchFinishTemporaryDetach(group.getChildAt(i));
            }
        }
    }

    protected int modifyFlingInitialVelocity(int initialVelocity) {
        return initialVelocity;
    }

    protected int getScrollChildTop() {
        final int count = getChildCount();
        if (count == 0) {
            return 0;
        }
        return getChildAt(0).getTop();
    }

    protected int getFirstChildTop() {
        final int count = getChildCount();
        if (count == 0) {
            return 0;
        }
        return getChildAt(0).getTop();
    }

    protected int getFillChildTop() {
        final int count = getChildCount();
        if (count == 0) {
            return 0;
        }
        return getChildAt(0).getTop();
    }

    protected int getFillChildBottom() {
        final int count = getChildCount();
        if (count == 0) {
            return 0;
        }
        return getChildAt(count - 1).getBottom();
    }

    protected int getScrollChildBottom() {
        final int count = getChildCount();
        if (count == 0) {
            return 0;
        }
        return getChildAt(count - 1).getBottom();
    }

    public interface OnScrollListener {
        public static int SCROLL_STATE_IDLE = 0;
        public static int SCROLL_STATE_TOUCH_SCROLL = 1;
        public static int SCROLL_STATE_FLING = 2;

        public void onScrollStateChanged(InnerAbsListView view, int scrollState);

        public void onScroll(InnerAbsListView view, int firstVisibleItem, int visibleItemCount, int totalItemCount);
    }

    public static interface RecyclerListener {
        void onMovedToScrapHeap(View view);
    }

    public static class LayoutParams extends ViewGroup.LayoutParams {

        public int viewType;
        public boolean recycledHeaderFooter;
        public boolean forceAdd;

        public LayoutParams(Context c, AttributeSet attrs) {
            super(c, attrs);
        }

        public LayoutParams(int w, int h) {
            super(w, h);
        }

        public LayoutParams(int w, int h, int viewType) {
            super(w, h);
            this.viewType = viewType;
        }

        public LayoutParams(ViewGroup.LayoutParams source) {
            super(source);
        }
    }

    private class WindowRunnnable {
        private int mOriginalAttachCount;

        public void rememberWindowAttachCount() {
            mOriginalAttachCount = getWindowAttachCount();
        }

        public boolean sameWindow() {
            return hasWindowFocus() && getWindowAttachCount() == mOriginalAttachCount;
        }
    }

    private class PerformClick extends WindowRunnnable implements Runnable {
        View mChild;
        int mClickMotionPosition;

        public void run() {
            if (mDataChanged) {
                return;
            }

            final ListAdapter adapter = mAdapter;
            final int motionPosition = mClickMotionPosition;
            if (adapter != null && mItemCount > 0 && motionPosition != INVALID_POSITION && motionPosition < adapter.getCount() && sameWindow()) {
                performItemClick(mChild, motionPosition, adapter.getItemId(motionPosition));
            }
        }
    }

    final class CheckForTap implements Runnable {
        public void run() {
            if (mTouchMode == TOUCH_MODE_DOWN) {
                mTouchMode = TOUCH_MODE_TAP;
                final View child = getChildAt(mMotionPosition - mFirstPosition);
                if (child != null && !child.hasFocusable()) {
                    mLayoutMode = LAYOUT_NORMAL;

                    if (!mDataChanged) {
                        layoutChildren();
                        child.setPressed(true);
                        positionSelector(child);
                        setPressed(true);

                        final int longPressTimeout = ViewConfiguration.getLongPressTimeout();
                        final boolean longClickable = isLongClickable();

                        if (mSelector != null) {
                            Drawable d = mSelector.getCurrent();
                            if (d != null && d instanceof TransitionDrawable) {
                                if (longClickable) {
                                    ((TransitionDrawable) d).startTransition(longPressTimeout);
                                } else {
                                    ((TransitionDrawable) d).resetTransition();
                                }
                            }
                        }

                        if (longClickable) {

                        } else {
                            mTouchMode = TOUCH_MODE_DONE_WAITING;
                        }
                    } else {
                        mTouchMode = TOUCH_MODE_DONE_WAITING;
                    }
                }
            }
        }
    }

    private class FlingRunnable implements Runnable {
        private final Scroller mScroller;
        private int mLastFlingY;

        FlingRunnable() {
            mScroller = new Scroller(getContext());
        }

        void start(int initialVelocity) {
            initialVelocity = modifyFlingInitialVelocity(initialVelocity);

            int initialY = initialVelocity < 0 ? Integer.MAX_VALUE : 0;
            mLastFlingY = initialY;
            mScroller.fling(0, initialY, 0, initialVelocity, 0, Integer.MAX_VALUE, 0, Integer.MAX_VALUE);

            mTouchMode = TOUCH_MODE_FLING;
            post(this);

            if (PROFILE_FLINGING) {
                if (!mFlingProfilingStarted) {
                    Debug.startMethodTracing("AbsListViewFling");
                    mFlingProfilingStarted = true;
                }
            }
        }

        void startScroll(int distance, int duration) {
            int initialY = distance < 0 ? Integer.MAX_VALUE : 0;
            mLastFlingY = initialY;
            mScroller.startScroll(0, initialY, 0, distance, duration);
            mTouchMode = TOUCH_MODE_FLING;
            post(this);
        }

        private void endFling() {
            mLastFlingY = 0;
            mTouchMode = TOUCH_MODE_REST;

            reportScrollStateChange(OnScrollListener.SCROLL_STATE_IDLE);
            clearScrollingCache();

            removeCallbacks(this);

            if (mPositionScroller != null) {
                removeCallbacks(mPositionScroller);
            }
            mScroller.forceFinished(true);
        }

        public void run() {
            switch (mTouchMode) {
                default:
                    return;

                case TOUCH_MODE_FLING: {
                    if (mItemCount == 0 || getChildCount() == 0) {
                        endFling();
                        return;
                    }

                    final Scroller scroller = mScroller;
                    boolean more = scroller.computeScrollOffset();
                    final int y = scroller.getCurrY();

                    int delta = mLastFlingY - y;
                    if (delta > 0) {
                        mMotionPosition = mFirstPosition;
                        mMotionViewOriginalTop = getScrollChildTop();
                        delta = Math.min(getHeight() - getPaddingBottom() - getPaddingTop() - 1, delta);
                    } else {
                        int offsetToLast = getChildCount() - 1;
                        mMotionPosition = mFirstPosition + offsetToLast;
                        mMotionViewOriginalTop = getScrollChildBottom();
                        delta = Math.max(-(getHeight() - getPaddingBottom() - getPaddingTop() - 1), delta);
                    }

                    final boolean atEnd = trackMotionScroll(delta, delta);

                    if (more && !atEnd) {
                        invalidate();
                        mLastFlingY = y;
                        post(this);
                    } else {
                        endFling();
                        if (PROFILE_FLINGING) {
                            if (mFlingProfilingStarted) {
                                Debug.stopMethodTracing();
                                mFlingProfilingStarted = false;
                            }
                        }
                    }
                    break;
                }
            }
        }
    }

    class PositionScroller implements Runnable {
        private static final int SCROLL_DURATION = 400;
        private static final int MOVE_DOWN_POS = 1;
        private static final int MOVE_UP_POS = 2;
        private static final int MOVE_DOWN_BOUND = 3;
        private static final int MOVE_UP_BOUND = 4;
        private final int mExtraScroll;
        private int mMode;
        private int mTargetPos;
        private int mBoundPos;
        private int mLastSeenPos;
        private int mScrollDuration;

        PositionScroller() {
            mExtraScroll = ViewConfiguration.get(getContext()).getScaledFadingEdgeLength();
        }

        void start(int position) {
            final int firstPos = mFirstPosition;
            final int lastPos = firstPos + getChildCount() - 1;

            int viewTravelCount = 0;
            if (position <= firstPos) {
                viewTravelCount = firstPos - position + 1;
                mMode = MOVE_UP_POS;
            } else if (position >= lastPos) {
                viewTravelCount = position - lastPos + 1;
                mMode = MOVE_DOWN_POS;
            } else {
                return;
            }

            if (viewTravelCount > 0) {
                mScrollDuration = SCROLL_DURATION / viewTravelCount;
            } else {
                mScrollDuration = SCROLL_DURATION;
            }
            mTargetPos = position;
            mBoundPos = INVALID_POSITION;
            mLastSeenPos = INVALID_POSITION;

            post(this);
        }

        void start(int position, int boundPosition) {
            if (boundPosition == INVALID_POSITION) {
                start(position);
                return;
            }

            final int firstPos = mFirstPosition;
            final int lastPos = firstPos + getChildCount() - 1;

            int viewTravelCount = 0;
            if (position <= firstPos) {
                final int boundPosFromLast = lastPos - boundPosition;
                if (boundPosFromLast < 1) {
                    return;
                }

                final int posTravel = firstPos - position + 1;
                final int boundTravel = boundPosFromLast - 1;
                if (boundTravel < posTravel) {
                    viewTravelCount = boundTravel;
                    mMode = MOVE_UP_BOUND;
                } else {
                    viewTravelCount = posTravel;
                    mMode = MOVE_UP_POS;
                }
            } else if (position >= lastPos) {
                final int boundPosFromFirst = boundPosition - firstPos;
                if (boundPosFromFirst < 1) {
                    return;
                }

                final int posTravel = position - lastPos + 1;
                final int boundTravel = boundPosFromFirst - 1;
                if (boundTravel < posTravel) {
                    viewTravelCount = boundTravel;
                    mMode = MOVE_DOWN_BOUND;
                } else {
                    viewTravelCount = posTravel;
                    mMode = MOVE_DOWN_POS;
                }
            } else {
                return;
            }

            if (viewTravelCount > 0) {
                mScrollDuration = SCROLL_DURATION / viewTravelCount;
            } else {
                mScrollDuration = SCROLL_DURATION;
            }
            mTargetPos = position;
            mBoundPos = boundPosition;
            mLastSeenPos = INVALID_POSITION;

            post(this);
        }

        void stop() {
            removeCallbacks(this);
        }

        public void run() {
            final int listHeight = getHeight();
            final int firstPos = mFirstPosition;

            switch (mMode) {
                case MOVE_DOWN_POS: {
                    final int lastViewIndex = getChildCount() - 1;
                    final int lastPos = firstPos + lastViewIndex;

                    if (lastViewIndex < 0) {
                        return;
                    }

                    if (lastPos == mLastSeenPos) {
                        post(this);
                        return;
                    }

                    final View lastView = getChildAt(lastViewIndex);
                    final int lastViewHeight = lastView.getHeight();
                    final int lastViewTop = lastView.getTop();
                    final int lastViewPixelsShowing = listHeight - lastViewTop;
                    final int extraScroll = lastPos < mItemCount - 1 ? mExtraScroll : mListPadding.bottom;

                    smoothScrollBy(lastViewHeight - lastViewPixelsShowing + extraScroll, mScrollDuration);

                    mLastSeenPos = lastPos;
                    if (lastPos < mTargetPos) {
                        post(this);
                    }
                    break;
                }

                case MOVE_DOWN_BOUND: {
                    final int nextViewIndex = 1;
                    final int childCount = getChildCount();

                    if (firstPos == mBoundPos || childCount <= nextViewIndex || firstPos + childCount >= mItemCount) {
                        return;
                    }
                    final int nextPos = firstPos + nextViewIndex;

                    if (nextPos == mLastSeenPos) {
                        post(this);
                        return;
                    }

                    final View nextView = getChildAt(nextViewIndex);
                    final int nextViewHeight = nextView.getHeight();
                    final int nextViewTop = nextView.getTop();
                    final int extraScroll = mExtraScroll;
                    if (nextPos < mBoundPos) {
                        smoothScrollBy(Math.max(0, nextViewHeight + nextViewTop - extraScroll), mScrollDuration);

                        mLastSeenPos = nextPos;

                        post(this);
                    } else {
                        if (nextViewTop > extraScroll) {
                            smoothScrollBy(nextViewTop - extraScroll, mScrollDuration);
                        }
                    }
                    break;
                }

                case MOVE_UP_POS: {
                    if (firstPos == mLastSeenPos) {
                        post(this);
                        return;
                    }

                    final View firstView = getChildAt(0);
                    if (firstView == null) {
                        return;
                    }
                    final int firstViewTop = firstView.getTop();
                    final int extraScroll = firstPos > 0 ? mExtraScroll : mListPadding.top;

                    smoothScrollBy(firstViewTop - extraScroll, mScrollDuration);

                    mLastSeenPos = firstPos;

                    if (firstPos > mTargetPos) {
                        post(this);
                    }
                    break;
                }

                case MOVE_UP_BOUND: {
                    final int lastViewIndex = getChildCount() - 2;
                    if (lastViewIndex < 0) {
                        return;
                    }
                    final int lastPos = firstPos + lastViewIndex;

                    if (lastPos == mLastSeenPos) {
                        post(this);
                        return;
                    }

                    final View lastView = getChildAt(lastViewIndex);
                    final int lastViewHeight = lastView.getHeight();
                    final int lastViewTop = lastView.getTop();
                    final int lastViewPixelsShowing = listHeight - lastViewTop;
                    mLastSeenPos = lastPos;
                    if (lastPos > mBoundPos) {
                        smoothScrollBy(-(lastViewPixelsShowing - mExtraScroll), mScrollDuration);
                        post(this);
                    } else {
                        final int bottom = listHeight - mExtraScroll;
                        final int lastViewBottom = lastViewTop + lastViewHeight;
                        if (bottom > lastViewBottom) {
                            smoothScrollBy(-(bottom - lastViewBottom), mScrollDuration);
                        }
                    }
                    break;
                }

                default:
                    break;
            }
        }
    }

    class RecycleBin {
        private RecyclerListener mRecyclerListener;
        private int mFirstActivePosition;
        private View[] mActiveViews = new View[0];
        private ArrayList<View>[] mScrapViews;
        private int mViewTypeCount;
        private ArrayList<View> mCurrentScrap;

        public void setViewTypeCount(int viewTypeCount) {
            if (viewTypeCount < 1) {
                throw new IllegalArgumentException("Can't have a viewTypeCount < 1");
            }
            @SuppressWarnings("unchecked")
            ArrayList<View>[] scrapViews = new ArrayList[viewTypeCount];
            for (int i = 0; i < viewTypeCount; i++) {
                scrapViews[i] = new ArrayList<View>();
            }
            mViewTypeCount = viewTypeCount;
            mCurrentScrap = scrapViews[0];
            mScrapViews = scrapViews;
        }

        public void markChildrenDirty() {
            if (mViewTypeCount == 1) {
                final ArrayList<View> scrap = mCurrentScrap;
                final int scrapCount = scrap.size();
                for (int i = 0; i < scrapCount; i++) {
                    scrap.get(i).forceLayout();
                }
            } else {
                final int typeCount = mViewTypeCount;
                for (int i = 0; i < typeCount; i++) {
                    final ArrayList<View> scrap = mScrapViews[i];
                    final int scrapCount = scrap.size();
                    for (int j = 0; j < scrapCount; j++) {
                        scrap.get(j).forceLayout();
                    }
                }
            }
        }

        public boolean shouldRecycleViewType(int viewType) {
            return viewType >= 0;
        }

        void clear() {
            if (mViewTypeCount == 1) {
                final ArrayList<View> scrap = mCurrentScrap;
                final int scrapCount = scrap.size();
                for (int i = 0; i < scrapCount; i++) {
                    removeDetachedView(scrap.remove(scrapCount - 1 - i), false);
                }
            } else {
                final int typeCount = mViewTypeCount;
                for (int i = 0; i < typeCount; i++) {
                    final ArrayList<View> scrap = mScrapViews[i];
                    final int scrapCount = scrap.size();
                    for (int j = 0; j < scrapCount; j++) {
                        removeDetachedView(scrap.remove(scrapCount - 1 - j), false);
                    }
                }
            }
        }

        void fillActiveViews(int childCount, int firstActivePosition) {
            if (mActiveViews.length < childCount) {
                mActiveViews = new View[childCount];
            }
            mFirstActivePosition = firstActivePosition;

            final View[] activeViews = mActiveViews;
            for (int i = 0; i < childCount; i++) {
                View child = getChildAt(i);
                InnerAbsListView.LayoutParams lp = (InnerAbsListView.LayoutParams) child.getLayoutParams();
                if (lp != null && lp.viewType != ITEM_VIEW_TYPE_HEADER_OR_FOOTER) {

                    activeViews[i] = child;
                }
            }
        }

        View getActiveView(int position) {
            int index = position - mFirstActivePosition;
            final View[] activeViews = mActiveViews;
            if (index >= 0 && index < activeViews.length) {
                final View match = activeViews[index];
                activeViews[index] = null;
                return match;
            }
            return null;
        }

        View getScrapView(int position) {
            ArrayList<View> scrapViews;
            if (mViewTypeCount == 1) {
                scrapViews = mCurrentScrap;
                int size = scrapViews.size();
                if (size > 0) {
                    return scrapViews.remove(size - 1);
                } else {
                    return null;
                }
            } else {
                int whichScrap = mAdapter.getItemViewType(position);
                if (whichScrap >= 0 && whichScrap < mScrapViews.length) {
                    scrapViews = mScrapViews[whichScrap];
                    int size = scrapViews.size();
                    if (size > 0) {
                        return scrapViews.remove(size - 1);
                    }
                }
            }
            return null;
        }

        void addScrapView(View scrap) {
            InnerAbsListView.LayoutParams lp = (InnerAbsListView.LayoutParams) scrap.getLayoutParams();
            if (lp == null) {
                return;
            }

            int viewType = lp.viewType;
            if (!shouldRecycleViewType(viewType)) {
                if (viewType != ITEM_VIEW_TYPE_HEADER_OR_FOOTER) {
                    removeDetachedView(scrap, false);
                }
                return;
            }

            if (mViewTypeCount == 1) {
                dispatchFinishTemporaryDetach(scrap);
                mCurrentScrap.add(scrap);
            } else {
                dispatchFinishTemporaryDetach(scrap);
                mScrapViews[viewType].add(scrap);
            }

            if (mRecyclerListener != null) {
                mRecyclerListener.onMovedToScrapHeap(scrap);
            }
        }

        void scrapActiveViews() {
            final View[] activeViews = mActiveViews;
            final boolean hasListener = mRecyclerListener != null;
            final boolean multipleScraps = mViewTypeCount > 1;

            ArrayList<View> scrapViews = mCurrentScrap;
            final int count = activeViews.length;
            for (int i = count - 1; i >= 0; i--) {
                final View victim = activeViews[i];
                if (victim != null) {
                    int whichScrap = ((InnerAbsListView.LayoutParams) victim.getLayoutParams()).viewType;

                    activeViews[i] = null;

                    if (!shouldRecycleViewType(whichScrap)) {
                        if (whichScrap != ITEM_VIEW_TYPE_HEADER_OR_FOOTER) {
                            removeDetachedView(victim, false);
                        }
                        continue;
                    }

                    if (multipleScraps) {
                        scrapViews = mScrapViews[whichScrap];
                    }
                    dispatchFinishTemporaryDetach(victim);
                    scrapViews.add(victim);

                    if (hasListener) {
                        mRecyclerListener.onMovedToScrapHeap(victim);
                    }

                    if (ViewDebug.TRACE_RECYCLER) {
                        ViewDebug.trace(victim, ViewDebug.RecyclerTraceType.MOVE_FROM_ACTIVE_TO_SCRAP_HEAP, mFirstActivePosition + i, -1);
                    }
                }
            }

            pruneScrapViews();
        }

        private void pruneScrapViews() {
            final int maxViews = mActiveViews.length;
            final int viewTypeCount = mViewTypeCount;
            final ArrayList<View>[] scrapViews = mScrapViews;
            for (int i = 0; i < viewTypeCount; ++i) {
                final ArrayList<View> scrapPile = scrapViews[i];
                int size = scrapPile.size();
                final int extras = size - maxViews;
                size--;
                for (int j = 0; j < extras; j++) {
                    removeDetachedView(scrapPile.remove(size--), false);
                }
            }
        }

        void reclaimScrapViews(List<View> views) {
            if (mViewTypeCount == 1) {
                views.addAll(mCurrentScrap);
            } else {
                final int viewTypeCount = mViewTypeCount;
                final ArrayList<View>[] scrapViews = mScrapViews;
                for (int i = 0; i < viewTypeCount; ++i) {
                    final ArrayList<View> scrapPile = scrapViews[i];
                    views.addAll(scrapPile);
                }
            }
        }

        void setCacheColorHint(int color) {
            if (mViewTypeCount == 1) {
                final ArrayList<View> scrap = mCurrentScrap;
                final int scrapCount = scrap.size();
                for (int i = 0; i < scrapCount; i++) {
                    scrap.get(i).setDrawingCacheBackgroundColor(color);
                }
            } else {
                final int typeCount = mViewTypeCount;
                for (int i = 0; i < typeCount; i++) {
                    final ArrayList<View> scrap = mScrapViews[i];
                    final int scrapCount = scrap.size();
                    for (int j = 0; j < scrapCount; j++) {
                        scrap.get(i).setDrawingCacheBackgroundColor(color);
                    }
                }
            }
            final View[] activeViews = mActiveViews;
            final int count = activeViews.length;
            for (int i = 0; i < count; ++i) {
                final View victim = activeViews[i];
                if (victim != null) {
                    victim.setDrawingCacheBackgroundColor(color);
                }
            }
        }
    }
}
