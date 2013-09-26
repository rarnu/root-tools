package com.rarnu.devlib.base.inner;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.PixelFormat;
import android.graphics.Rect;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.util.SparseBooleanArray;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewDebug;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityEvent;
import android.widget.ListAdapter;
import com.rarnu.devlib.R;

import java.util.ArrayList;

public class InnerListView extends InnerAbsListView {

    static final int NO_POSITION = -1;
    private static final float MAX_SCROLL_FACTOR = 0.33f;
    private final Rect mTempRect = new Rect();
    Drawable mDivider;
    int mDividerHeight;
    Drawable mOverScrollHeader;
    Drawable mOverScrollFooter;
    private ArrayList<FixedViewInfo> mHeaderViewInfos = new ArrayList<InnerListView.FixedViewInfo>();
    private ArrayList<FixedViewInfo> mFooterViewInfos = new ArrayList<InnerListView.FixedViewInfo>();
    private boolean mIsCacheColorOpaque;
    private boolean mDividerIsOpaque;
    private boolean mClipDivider;
    private boolean mHeaderDividersEnabled;
    private boolean mFooterDividersEnabled;
    private boolean mAreAllItemsSelectable = true;
    private boolean mItemsCanFocus = false;
    private Paint mDividerPaint;

    public InnerListView(Context context) {
        this(context, null);
    }

    public InnerListView(Context context, AttributeSet attrs) {
        this(context, attrs, R.attr.listViewStyle);
    }

    public InnerListView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);

        TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.ListView, defStyle, 0);

        final Drawable osHeader = a.getDrawable(R.styleable.ListView_overScrollHeader);
        if (osHeader != null) {
            setOverscrollHeader(osHeader);
        }

        final Drawable osFooter = a.getDrawable(R.styleable.ListView_overScrollFooter);
        if (osFooter != null) {
            setOverscrollFooter(osFooter);
        }

        final int dividerHeight = a.getDimensionPixelSize(R.styleable.ListView_dividerHeight, 0);
        if (dividerHeight != 0) {
            setDividerHeight(dividerHeight);
        }

        mHeaderDividersEnabled = a.getBoolean(R.styleable.ListView_headerDividersEnabled, true);
        mFooterDividersEnabled = a.getBoolean(R.styleable.ListView_footerDividersEnabled, true);

        a.recycle();
    }

    public int getMaxScrollAmount() {
        return (int) (MAX_SCROLL_FACTOR * (getBottom() - getTop()));
    }

    private void adjustViewsUpOrDown() {
        final int childCount = getChildCount();
        int delta;

        if (childCount > 0) {
            if (!mStackFromBottom) {
                final int firstTop = getScrollChildTop();
                delta = firstTop - mListPadding.top;
                if (mFirstPosition != 0) {
                    delta -= mDividerHeight;
                }
                if (delta < 0) {
                    delta = 0;
                }
            } else {
                final int lastBottom = getScrollChildBottom();
                delta = lastBottom - (getHeight() - mListPadding.bottom);

                if (mFirstPosition + childCount < mItemCount) {
                    delta += mDividerHeight;
                }

                if (delta > 0) {
                    delta = 0;
                }
            }

            if (delta != 0) {
                tryOffsetChildrenTopAndBottom(-delta);
            }
        }
    }

    public void addHeaderView(View v, Object data, boolean isSelectable) {

        if (mAdapter != null) {
            throw new IllegalStateException("Cannot add header view to list -- setAdapter has already been called.");
        }

        FixedViewInfo info = new FixedViewInfo();
        info.view = v;
        info.data = data;
        info.isSelectable = isSelectable;
        mHeaderViewInfos.add(info);
    }

    public void addHeaderView(View v) {
        addHeaderView(v, null, true);
    }

    @Override
    public int getHeaderViewsCount() {
        return mHeaderViewInfos.size();
    }

    public boolean isFixedView(View v) {

        {
            ArrayList<FixedViewInfo> where = mHeaderViewInfos;
            int len = where.size();
            for (int i = 0; i < len; ++i) {
                FixedViewInfo info = where.get(i);
                if (info.view == v) {
                    return true;
                }
            }
        }

        {
            ArrayList<FixedViewInfo> where = mFooterViewInfos;
            int len = where.size();
            for (int i = 0; i < len; ++i) {
                FixedViewInfo info = where.get(i);
                if (info.view == v) {
                    return true;
                }
            }
        }

        return false;
    }

    public boolean removeHeaderView(View v) {
        if (mHeaderViewInfos.size() > 0) {
            boolean result = false;
            if (((InnerHeaderViewAdapter) mAdapter).removeHeader(v)) {
                mDataSetObserver.onChanged();
                result = true;
            }
            removeFixedViewInfo(v, mHeaderViewInfos);
            return result;
        }
        return false;
    }

    private void removeFixedViewInfo(View v, ArrayList<FixedViewInfo> where) {
        int len = where.size();
        for (int i = 0; i < len; ++i) {
            FixedViewInfo info = where.get(i);
            if (info.view == v) {
                where.remove(i);
                break;
            }
        }
    }

    public void addFooterView(View v, Object data, boolean isSelectable) {
        FixedViewInfo info = new FixedViewInfo();
        info.view = v;
        info.data = data;
        info.isSelectable = isSelectable;
        mFooterViewInfos.add(info);

        if (mDataSetObserver != null) {
            mDataSetObserver.onChanged();
        }
    }

    public void addFooterView(View v) {
        addFooterView(v, null, true);
    }

    @Override
    public int getFooterViewsCount() {
        return mFooterViewInfos.size();
    }

    public boolean removeFooterView(View v) {
        if (mFooterViewInfos.size() > 0) {
            boolean result = false;
            if (((InnerHeaderViewAdapter) mAdapter).removeFooter(v)) {
                mDataSetObserver.onChanged();
                result = true;
            }
            removeFixedViewInfo(v, mFooterViewInfos);
            return result;
        }
        return false;
    }

    @Override
    public ListAdapter getAdapter() {
        return mAdapter;
    }

    @Override
    public void setAdapter(ListAdapter adapter) {
        if (null != mAdapter) {
            mAdapter.unregisterDataSetObserver(mDataSetObserver);
        }

        resetList();
        mRecycler.clear();

        if (mHeaderViewInfos.size() > 0 || mFooterViewInfos.size() > 0) {
            mAdapter = new InnerHeaderViewAdapter(mHeaderViewInfos, mFooterViewInfos, adapter);
        } else {
            mAdapter = adapter;
        }

        mOldSelectedPosition = INVALID_POSITION;
        mOldSelectedRowId = INVALID_ROW_ID;
        if (mAdapter != null) {
            mAreAllItemsSelectable = mAdapter.areAllItemsEnabled();
            mOldItemCount = mItemCount;
            mItemCount = mAdapter.getCount();
            checkFocus();

            mDataSetObserver = new AdapterDataSetObserver();
            mAdapter.registerDataSetObserver(mDataSetObserver);

            mRecycler.setViewTypeCount(mAdapter.getViewTypeCount());

        } else {
            mAreAllItemsSelectable = true;
            checkFocus();
        }

        requestLayout();
    }

    @Override
    public int getFirstVisiblePosition() {
        return Math.max(0, mFirstPosition - getHeaderViewsCount());
    }

    @Override
    public int getLastVisiblePosition() {
        return Math.min(mFirstPosition + getChildCount() - 1, mAdapter.getCount() - 1);
    }

    @Override
    void resetList() {
        clearRecycledState(mHeaderViewInfos);
        clearRecycledState(mFooterViewInfos);

        super.resetList();

        mLayoutMode = LAYOUT_NORMAL;
    }

    private void clearRecycledState(ArrayList<FixedViewInfo> infos) {
        if (infos != null) {
            final int count = infos.size();

            for (int i = 0; i < count; i++) {
                final View child = infos.get(i).view;
                final LayoutParams p = (LayoutParams) child.getLayoutParams();
                if (p != null) {
                    p.recycledHeaderFooter = false;
                }
            }
        }
    }

    private boolean showingTopFadingEdge() {
        final int listTop = getScrollY() + mListPadding.top;
        return (mFirstPosition > 0) || (getChildAt(0).getTop() > listTop);
    }

    private boolean showingBottomFadingEdge() {
        final int childCount = getChildCount();
        final int bottomOfBottomChild = getChildAt(childCount - 1).getBottom();
        final int lastVisiblePosition = mFirstPosition + childCount - 1;
        final int listBottom = getScrollY() + getHeight() - mListPadding.bottom;

        return (lastVisiblePosition < mItemCount - 1) || (bottomOfBottomChild < listBottom);
    }

    @Override
    public boolean requestChildRectangleOnScreen(View child, Rect rect, boolean immediate) {

        int rectTopWithinChild = rect.top;

        rect.offset(child.getLeft(), child.getTop());
        rect.offset(-child.getScrollX(), -child.getScrollY());

        final int height = getHeight();
        int listUnfadedTop = getScrollY();
        int listUnfadedBottom = listUnfadedTop + height;
        final int fadingEdge = getVerticalFadingEdgeLength();

        if (showingTopFadingEdge()) {
            if (rectTopWithinChild > fadingEdge) {
                listUnfadedTop += fadingEdge;
            }
        }

        int childCount = getChildCount();
        int bottomOfBottomChild = getChildAt(childCount - 1).getBottom();

        if (showingBottomFadingEdge()) {
            if (rect.bottom < (bottomOfBottomChild - fadingEdge)) {
                listUnfadedBottom -= fadingEdge;
            }
        }

        int scrollYDelta = 0;

        if (rect.bottom > listUnfadedBottom && rect.top > listUnfadedTop) {

            if (rect.height() > height) {
                scrollYDelta += (rect.top - listUnfadedTop);
            } else {
                scrollYDelta += (rect.bottom - listUnfadedBottom);
            }
            int distanceToBottom = bottomOfBottomChild - listUnfadedBottom;
            scrollYDelta = Math.min(scrollYDelta, distanceToBottom);
        } else if (rect.top < listUnfadedTop && rect.bottom < listUnfadedBottom) {

            if (rect.height() > height) {
                scrollYDelta -= (listUnfadedBottom - rect.bottom);
            } else {
                scrollYDelta -= (listUnfadedTop - rect.top);
            }

            int top = getChildAt(0).getTop();
            int deltaToTop = top - listUnfadedTop;
            scrollYDelta = Math.max(scrollYDelta, deltaToTop);
        }

        final boolean scroll = scrollYDelta != 0;
        if (scroll) {
            scrollListItemsBy(-scrollYDelta);
            positionSelector(child);
            mSelectedTop = child.getTop();
            invalidate();
        }
        return scroll;
    }

    protected int getItemLeft(int pos) {
        return mListPadding.left;
    }

    protected int getItemTop(int pos) {
        int count = getChildCount();
        return count > 0 ? getChildAt(count - 1).getBottom() + mDividerHeight : getListPaddingTop();
    }

    protected int getItemBottom(int pos) {
        int count = getChildCount();
        return count > 0 ? getChildAt(0).getTop() - mDividerHeight : getHeight() - getListPaddingBottom();
    }

    @Override
    protected void fillGap(boolean down) {
        final int count = getChildCount();
        if (down) {
            fillDown(mFirstPosition + count, getItemTop(mFirstPosition + count));
            onAdjustChildViews(down);
        } else {
            fillUp(mFirstPosition - 1, getItemBottom(mFirstPosition - 1));
            onAdjustChildViews(down);
        }
    }

    private View fillDown(int pos, int top) {

        int end = (getBottom() - getTop()) - mListPadding.bottom;
        int childTop = getFillChildBottom() + mDividerHeight;

        while (childTop < end && pos < mItemCount) {
            makeAndAddView(pos, getItemTop(pos), true, false);
            pos++;
            childTop = getFillChildBottom() + mDividerHeight;
        }

        return null;
    }

    private View fillUp(int pos, int bottom) {
        int end = mListPadding.top;
        int childBottom = getFillChildTop();
        while (childBottom > end && pos >= 0) {
            makeAndAddView(pos, getItemBottom(pos), false, false);
            pos--;
            childBottom = getItemBottom(pos);
        }

        mFirstPosition = pos + 1;

        return null;
    }

    private View fillFromTop(int nextTop) {
        mFirstPosition = Math.min(mFirstPosition, -1);
        mFirstPosition = Math.min(mFirstPosition, mItemCount - 1);
        if (mFirstPosition < 0) {
            mFirstPosition = 0;
        }
        return fillDown(mFirstPosition, nextTop);
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);

        int widthMode = MeasureSpec.getMode(widthMeasureSpec);
        int heightMode = MeasureSpec.getMode(heightMeasureSpec);
        int widthSize = MeasureSpec.getSize(widthMeasureSpec);
        int heightSize = MeasureSpec.getSize(heightMeasureSpec);

        int childWidth = 0;
        int childHeight = 0;

        mItemCount = mAdapter == null ? 0 : mAdapter.getCount();
        if (mItemCount > 0 && (widthMode == MeasureSpec.UNSPECIFIED || heightMode == MeasureSpec.UNSPECIFIED)) {
            final View child = obtainView(0, mIsScrap);

            measureScrapChild(child, 0, widthMeasureSpec);

            childWidth = child.getMeasuredWidth();
            childHeight = child.getMeasuredHeight();

            if (recycleOnMeasure() && mRecycler.shouldRecycleViewType(((LayoutParams) child.getLayoutParams()).viewType)) {
                mRecycler.addScrapView(child);
            }
        }

        if (widthMode == MeasureSpec.UNSPECIFIED) {
            widthSize = mListPadding.left + mListPadding.right + childWidth + getVerticalScrollbarWidth();
        }

        if (heightMode == MeasureSpec.UNSPECIFIED) {
            heightSize = mListPadding.top + mListPadding.bottom + childHeight + getVerticalFadingEdgeLength() * 2;
        }

        if (heightMode == MeasureSpec.AT_MOST) {
            heightSize = measureHeightOfChildren(widthMeasureSpec, 0, NO_POSITION, heightSize, -1);
        }

        setMeasuredDimension(widthSize, heightSize);
        mWidthMeasureSpec = widthMeasureSpec;
    }

    private void measureScrapChild(View child, int position, int widthMeasureSpec) {
        LayoutParams p = (LayoutParams) child.getLayoutParams();
        if (p == null) {
            p = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT, 0);
            child.setLayoutParams(p);
        }
        p.viewType = mAdapter.getItemViewType(position);
        p.forceAdd = true;

        int childWidthSpec = ViewGroup.getChildMeasureSpec(widthMeasureSpec, mListPadding.left + mListPadding.right, p.width);
        int lpHeight = p.height;
        int childHeightSpec;
        if (lpHeight > 0) {
            childHeightSpec = MeasureSpec.makeMeasureSpec(lpHeight, MeasureSpec.EXACTLY);
        } else {
            childHeightSpec = MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED);
        }
        child.measure(childWidthSpec, childHeightSpec);
    }

    protected boolean recycleOnMeasure() {
        return true;
    }

    final int measureHeightOfChildren(int widthMeasureSpec, int startPosition, int endPosition, final int maxHeight, int disallowPartialChildPosition) {

        final ListAdapter adapter = mAdapter;
        if (adapter == null) {
            return mListPadding.top + mListPadding.bottom;
        }

        int returnedHeight = mListPadding.top + mListPadding.bottom;
        final int dividerHeight = ((mDividerHeight > 0) && mDivider != null) ? mDividerHeight : 0;

        int prevHeightWithoutPartialChild = 0;
        int i;
        View child;

        endPosition = (endPosition == NO_POSITION) ? adapter.getCount() - 1 : endPosition;
        final InnerAbsListView.RecycleBin recycleBin = mRecycler;
        final boolean recyle = recycleOnMeasure();
        final boolean[] isScrap = mIsScrap;

        for (i = startPosition; i <= endPosition; ++i) {
            child = obtainView(i, isScrap);

            measureScrapChild(child, i, widthMeasureSpec);

            if (i > 0) {
                returnedHeight += dividerHeight;
            }

            if (recyle && recycleBin.shouldRecycleViewType(((LayoutParams) child.getLayoutParams()).viewType)) {
                recycleBin.addScrapView(child);
            }

            returnedHeight += child.getMeasuredHeight();

            if (returnedHeight >= maxHeight) {
                return (disallowPartialChildPosition >= 0) && (i > disallowPartialChildPosition) && (prevHeightWithoutPartialChild > 0) && (returnedHeight != maxHeight) ? prevHeightWithoutPartialChild : maxHeight;
            }

            if ((disallowPartialChildPosition >= 0) && (i >= disallowPartialChildPosition)) {
                prevHeightWithoutPartialChild = returnedHeight;
            }
        }

        return returnedHeight;
    }

    @Override
    int findMotionRow(int y) {
        int childCount = getChildCount();
        if (childCount > 0) {
            if (!mStackFromBottom) {
                for (int i = 0; i < childCount; i++) {
                    View v = getChildAt(i);
                    if (y <= v.getBottom()) {
                        return mFirstPosition + i;
                    }
                }
            } else {
                for (int i = childCount - 1; i >= 0; i--) {
                    View v = getChildAt(i);
                    if (y >= v.getTop()) {
                        return mFirstPosition + i;
                    }
                }
            }
        }
        return INVALID_POSITION;
    }

    private View fillSpecific(int position, int top) {

        View temp = makeAndAddView(position, top, true, false);

        mFirstPosition = position;
        final int dividerHeight = mDividerHeight;
        if (!mStackFromBottom) {
            fillUp(position - 1, temp.getTop() - dividerHeight);
            adjustViewsUpOrDown();
            fillDown(position + 1, temp.getBottom() + dividerHeight);
            int childCount = getChildCount();
            if (childCount > 0) {
                correctTooHigh(childCount);
            }
        } else {
            fillDown(position + 1, temp.getBottom() + dividerHeight);
            adjustViewsUpOrDown();
            fillUp(position - 1, temp.getTop() - dividerHeight);
            int childCount = getChildCount();
            if (childCount > 0) {
                correctTooLow(childCount);
            }
        }

        return null;
    }

    private void correctTooHigh(int childCount) {

        int lastPosition = mFirstPosition + childCount - 1;
        if (lastPosition == mItemCount - 1 && childCount > 0) {
            final int lastBottom = getScrollChildBottom();
            final int end = (getBottom() - getTop()) - mListPadding.bottom;
            int bottomOffset = end - lastBottom;
            final int firstTop = getScrollChildTop();
            if (bottomOffset > 0 && (mFirstPosition > 0 || firstTop < mListPadding.top)) {
                if (mFirstPosition == 0) {
                    bottomOffset = Math.min(bottomOffset, mListPadding.top - firstTop);
                }
                tryOffsetChildrenTopAndBottom(bottomOffset);
                if (mFirstPosition > 0) {
                    int newFirstTop = getScrollChildTop();
                    fillUp(mFirstPosition - 1, newFirstTop - mDividerHeight);
                    adjustViewsUpOrDown();
                }

            }
        }
    }

    private void correctTooLow(int childCount) {

        if (mFirstPosition == 0 && childCount > 0) {
            final int firstTop = getScrollChildTop();
            final int start = mListPadding.top;
            final int end = (getBottom() - getTop()) - mListPadding.bottom;
            int topOffset = firstTop - start;
            final int lastBottom = getScrollChildBottom();

            int lastPosition = mFirstPosition + childCount - 1;

            if (topOffset > 0) {
                if (lastPosition < mItemCount - 1 || lastBottom > end) {
                    if (lastPosition == mItemCount - 1) {
                        topOffset = Math.min(topOffset, lastBottom - end);
                    }
                    tryOffsetChildrenTopAndBottom(-topOffset);
                    if (lastPosition < mItemCount - 1) {
                        fillDown(lastPosition + 1, getFillChildTop()
                                + mDividerHeight);
                        adjustViewsUpOrDown();
                    }
                } else if (lastPosition == mItemCount - 1) {
                    adjustViewsUpOrDown();
                }
            }
        }
    }

    @Override
    protected void layoutChildren() {
        final boolean blockLayoutRequests = mBlockLayoutRequests;
        if (!blockLayoutRequests) {
            mBlockLayoutRequests = true;
        } else {
            return;
        }

        try {
            super.layoutChildren();
            invalidate();
            if (mAdapter == null) {
                resetList();
                invokeOnItemScrollListener();
                return;
            }

            int childrenTop = mListPadding.top;
            int childrenBottom = getBottom() - getTop() - mListPadding.bottom;

            int childCount = getChildCount();
            int index = 0;

            View oldFirst = null;
            View focusLayoutRestoreView = null;

            switch (mLayoutMode) {
                case LAYOUT_FORCE_TOP:
                case LAYOUT_FORCE_BOTTOM:
                case LAYOUT_SPECIFIC:
                case LAYOUT_SYNC:
                    break;
                default:
                    oldFirst = getChildAt(0);
            }

            boolean dataChanged = mDataChanged;
            if (dataChanged) {
                handleDataChanged();
            }

            if (mItemCount == 0) {
                resetList();
                invokeOnItemScrollListener();
                return;
            } else if (mItemCount != mAdapter.getCount()) {
                throw new IllegalStateException("The content of the adapter has changed but ListView did not receive a notification. Make sure the content of your adapter is not modified from a background thread, but only from the UI thread. [in ListView(" + getId() + ", " + getClass() + ") with Adapter(" + mAdapter.getClass() + ")]");
            }

            final int firstPosition = mFirstPosition;
            final RecycleBin recycleBin = mRecycler;

            if (dataChanged) {
                for (int i = 0; i < childCount; i++) {
                    recycleBin.addScrapView(getChildAt(i));
                    if (ViewDebug.TRACE_RECYCLER) {
                        ViewDebug.trace(getChildAt(i), ViewDebug.RecyclerTraceType.MOVE_TO_SCRAP_HEAP, index, i);
                    }
                }
            } else {
                recycleBin.fillActiveViews(childCount, firstPosition);
            }

            final View focusedChild = getFocusedChild();
            if (focusedChild != null) {

                if (!dataChanged || isDirectChildHeaderOrFooter(focusedChild)) {
                    focusLayoutRestoreView = findFocus();
                    if (focusLayoutRestoreView != null) {
                        focusLayoutRestoreView.onStartTemporaryDetach();
                    }
                }
                requestFocus();
            }

            switch (mLayoutMode) {
                case LAYOUT_SYNC:
                    onLayoutSync(mSyncPosition);
                    detachAllViewsFromParent();
                    fillSpecific(mSyncPosition, mSpecificTop);
                    onLayoutSyncFinished(mSyncPosition);
                    break;
                case LAYOUT_FORCE_BOTTOM:
                    detachAllViewsFromParent();
                    fillUp(mItemCount - 1, childrenBottom);
                    adjustViewsUpOrDown();
                    break;
                case LAYOUT_FORCE_TOP:
                    detachAllViewsFromParent();
                    mFirstPosition = 0;
                    fillFromTop(childrenTop);
                    adjustViewsUpOrDown();
                    break;
                default:
                    if (childCount == 0) {
                        detachAllViewsFromParent();
                        if (!mStackFromBottom) {
                            fillFromTop(childrenTop);
                        } else {
                            fillUp(mItemCount - 1, childrenBottom);
                        }
                    } else {
                        if (mFirstPosition < mItemCount) {
                            onLayoutSync(mFirstPosition);
                            detachAllViewsFromParent();
                            fillSpecific(mFirstPosition, oldFirst == null ? childrenTop : oldFirst.getTop());
                            onLayoutSyncFinished(mFirstPosition);
                        } else {
                            onLayoutSync(0);
                            detachAllViewsFromParent();
                            fillSpecific(0, childrenTop);
                            onLayoutSyncFinished(0);
                        }
                    }
                    break;
            }

            recycleBin.scrapActiveViews();

            if (mTouchMode > TOUCH_MODE_DOWN && mTouchMode < TOUCH_MODE_SCROLL) {
                View child = getChildAt(mMotionPosition - mFirstPosition);
                if (child != null)
                    positionSelector(child);
            } else {
                mSelectedTop = 0;
                mSelectorRect.setEmpty();
            }

            if (hasFocus() && focusLayoutRestoreView != null) {
                focusLayoutRestoreView.requestFocus();
            }

            if (focusLayoutRestoreView != null && focusLayoutRestoreView.getWindowToken() != null) {
                focusLayoutRestoreView.onFinishTemporaryDetach();
            }

            mLayoutMode = LAYOUT_NORMAL;
            mDataChanged = false;
            mNeedSync = false;

            invokeOnItemScrollListener();
        } finally {
            if (!blockLayoutRequests) {
                mBlockLayoutRequests = false;
            }
        }
    }

    private boolean isDirectChildHeaderOrFooter(View child) {

        final ArrayList<FixedViewInfo> headers = mHeaderViewInfos;
        final int numHeaders = headers.size();
        for (int i = 0; i < numHeaders; i++) {
            if (child == headers.get(i).view) {
                return true;
            }
        }
        final ArrayList<FixedViewInfo> footers = mFooterViewInfos;
        final int numFooters = footers.size();
        for (int i = 0; i < numFooters; i++) {
            if (child == footers.get(i).view) {
                return true;
            }
        }
        return false;
    }

    private View makeAndAddView(int position, int childrenBottomOrTop, boolean flow, boolean selected) {
        View child;

        int childrenLeft;
        if (!mDataChanged) {
            child = mRecycler.getActiveView(position);
            if (child != null) {

                if (ViewDebug.TRACE_RECYCLER) {
                    ViewDebug.trace(child, ViewDebug.RecyclerTraceType.RECYCLE_FROM_ACTIVE_HEAP, position, getChildCount());
                }

                childrenLeft = getItemLeft(position);
                setupChild(child, position, childrenBottomOrTop, flow,
                        childrenLeft, selected, true);
                return child;
            }
        }

        onItemAddedToList(position, flow);
        childrenLeft = getItemLeft(position);

        child = obtainView(position, mIsScrap);

        setupChild(child, position, childrenBottomOrTop, flow, childrenLeft, selected, mIsScrap[0]);

        return child;
    }

    protected void onItemAddedToList(int position, boolean flow) {
    }

    private void setupChild(View child, int position, int y, boolean flowDown, int childrenLeft, boolean selected, boolean recycled) {

        final boolean isSelected = selected && shouldShowSelector();
        final boolean updateChildSelected = isSelected != child.isSelected();
        final int mode = mTouchMode;
        final boolean isPressed = mode > TOUCH_MODE_DOWN && mode < TOUCH_MODE_SCROLL && mMotionPosition == position;
        final boolean updateChildPressed = isPressed != child.isPressed();
        final boolean needToMeasure = !recycled || updateChildSelected || child.isLayoutRequested();

        InnerAbsListView.LayoutParams p = (InnerAbsListView.LayoutParams) child.getLayoutParams();
        if (p == null) {
            p = new InnerAbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT, 0);
        }
        p.viewType = mAdapter.getItemViewType(position);

        if ((recycled && !p.forceAdd) || (p.recycledHeaderFooter && p.viewType == InnerAdapterView.ITEM_VIEW_TYPE_HEADER_OR_FOOTER)) {
            attachViewToParent(child, flowDown ? -1 : 0, p);
        } else {
            p.forceAdd = false;
            if (p.viewType == InnerAdapterView.ITEM_VIEW_TYPE_HEADER_OR_FOOTER) {
                p.recycledHeaderFooter = true;
            }
            addViewInLayout(child, flowDown ? -1 : 0, p, true);
        }

        if (updateChildSelected) {
            child.setSelected(isSelected);
        }

        if (updateChildPressed) {
            child.setPressed(isPressed);
        }

        if (needToMeasure) {
            int childWidthSpec = ViewGroup.getChildMeasureSpec(mWidthMeasureSpec, mListPadding.left + mListPadding.right, p.width);
            int lpHeight = p.height;
            int childHeightSpec;
            if (lpHeight > 0) {
                childHeightSpec = MeasureSpec.makeMeasureSpec(lpHeight, MeasureSpec.EXACTLY);
            } else {
                childHeightSpec = MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED);
            }

            onMeasureChild(child, position, childWidthSpec, childHeightSpec);
        } else {
            cleanupLayoutState(child);
        }

        final int w = child.getMeasuredWidth();
        final int h = child.getMeasuredHeight();
        final int childTop = flowDown ? y : y - h;

        if (needToMeasure) {
            final int childRight = childrenLeft + w;
            final int childBottom = childTop + h;
            onLayoutChild(child, position, childrenLeft, childTop, childRight, childBottom);
        } else {
            final int offsetLeft = childrenLeft - child.getLeft();
            final int offsetTop = childTop - child.getTop();
            onOffsetChild(child, position, offsetLeft, offsetTop);
        }

        if (mCachingStarted && !child.isDrawingCacheEnabled()) {
            child.setDrawingCacheEnabled(true);
        }
    }

    protected void onOffsetChild(View child, int position, int offsetLeft, int offsetTop) {
        child.offsetLeftAndRight(offsetLeft);
        child.offsetTopAndBottom(offsetTop);
    }

    protected void onLayoutChild(View child, int position, int l, int t, int r, int b) {
        child.layout(l, t, r, b);
    }

    protected void onMeasureChild(View child, int position, int widthMeasureSpec, int heightMeasureSpec) {
        child.measure(widthMeasureSpec, heightMeasureSpec);
    }

    protected void onAdjustChildViews(boolean down) {
        if (down) {
            correctTooHigh(getChildCount());
        } else {
            correctTooLow(getChildCount());
        }
    }

    @Override
    protected boolean canAnimate() {
        return super.canAnimate() && mItemCount > 0;
    }

    @Override
    public void setSelection(int position) {
    }

    @Override
    int lookForSelectablePosition(int position, boolean lookDown) {
        final ListAdapter adapter = mAdapter;
        if (adapter == null || isInTouchMode()) {
            return INVALID_POSITION;
        }

        final int count = adapter.getCount();
        if (!mAreAllItemsSelectable) {
            if (lookDown) {
                position = Math.max(0, position);
                while (position < count && !adapter.isEnabled(position)) {
                    position++;
                }
            } else {
                position = Math.min(position, count - 1);
                while (position >= 0 && !adapter.isEnabled(position)) {
                    position--;
                }
            }

            if (position < 0 || position >= count) {
                return INVALID_POSITION;
            }
            return position;
        } else {
            if (position < 0 || position >= count) {
                return INVALID_POSITION;
            }
            return position;
        }
    }

    @Override
    public boolean dispatchPopulateAccessibilityEvent(AccessibilityEvent event) {
        boolean populated = super.dispatchPopulateAccessibilityEvent(event);

        if (!populated) {
            int itemCount = 0;
            int currentItemIndex = getSelectedItemPosition();

            ListAdapter adapter = getAdapter();
            if (adapter != null) {
                final int count = adapter.getCount();
                if (count < 15) {
                    for (int i = 0; i < count; i++) {
                        if (adapter.isEnabled(i)) {
                            itemCount++;
                        } else if (i <= currentItemIndex) {
                            currentItemIndex--;
                        }
                    }
                } else {
                    itemCount = count;
                }
            }

            event.setItemCount(itemCount);
            event.setCurrentItemIndex(currentItemIndex);
        }

        return populated;
    }

    public boolean fullScroll(int direction) {
        boolean moved = false;
        if (direction == FOCUS_UP) {
            int position = lookForSelectablePosition(0, true);
            if (position >= 0) {
                mLayoutMode = LAYOUT_FORCE_TOP;
                invokeOnItemScrollListener();
                moved = true;
            }
        } else if (direction == FOCUS_DOWN) {
            int position = lookForSelectablePosition(mItemCount - 1, true);
            if (position >= 0) {
                mLayoutMode = LAYOUT_FORCE_BOTTOM;
                invokeOnItemScrollListener();
            }
            moved = true;
        }

        if (moved && !awakenScrollBars()) {
            awakenScrollBars();
            invalidate();
        }

        return moved;
    }

    private void scrollListItemsBy(int amount) {
        tryOffsetChildrenTopAndBottom(amount);

        final int listBottom = getHeight() - mListPadding.bottom;
        final int listTop = mListPadding.top;
        final InnerAbsListView.RecycleBin recycleBin = mRecycler;

        if (amount < 0) {

            View last = getLastChild();
            int numChildren = getChildCount();

            while (last.getBottom() < listBottom) {
                final int lastVisiblePosition = mFirstPosition + numChildren - 1;
                if (lastVisiblePosition < mItemCount - 1) {
                    addViewBelow(last, lastVisiblePosition);
                    last = getLastChild();
                    numChildren++;
                } else {
                    break;
                }
            }

            if (last.getBottom() < listBottom) {
                tryOffsetChildrenTopAndBottom(listBottom - last.getBottom());
            }

            View first = getChildAt(0);
            while (first.getBottom() < listTop) {
                InnerAbsListView.LayoutParams layoutParams = (LayoutParams) first.getLayoutParams();
                if (recycleBin.shouldRecycleViewType(layoutParams.viewType)) {
                    detachViewFromParent(first);
                    recycleBin.addScrapView(first);
                } else {
                    removeViewInLayout(first);
                }
                first = getChildAt(0);
                mFirstPosition++;
            }
        } else {

            View first = getChildAt(0);

            while ((first.getTop() > listTop) && (mFirstPosition > 0)) {
                first = addViewAbove(first, mFirstPosition);
                mFirstPosition--;
            }

            if (first.getTop() > listTop) {
                tryOffsetChildrenTopAndBottom(listTop - first.getTop());
            }

            int lastIndex = getChildCount() - 1;
            View last = getChildAt(lastIndex);

            while (last.getTop() > listBottom) {
                InnerAbsListView.LayoutParams layoutParams = (LayoutParams) last.getLayoutParams();
                if (recycleBin.shouldRecycleViewType(layoutParams.viewType)) {
                    detachViewFromParent(last);
                    recycleBin.addScrapView(last);
                } else {
                    removeViewInLayout(last);
                }
                last = getChildAt(--lastIndex);
            }
        }
    }

    protected View getLastChild() {
        int numChildren = getChildCount();
        return getChildAt(numChildren - 1);
    }

    private View addViewAbove(View theView, int position) {
        int abovePosition = position - 1;
        View view = obtainView(abovePosition, mIsScrap);
        int edgeOfNewChild = theView.getTop() - mDividerHeight;
        setupChild(view, abovePosition, edgeOfNewChild, false, mListPadding.left, false, mIsScrap[0]);
        return view;
    }

    private View addViewBelow(View theView, int position) {
        int belowPosition = position + 1;
        View view = obtainView(belowPosition, mIsScrap);
        int edgeOfNewChild = theView.getBottom() + mDividerHeight;
        setupChild(view, belowPosition, edgeOfNewChild, true, mListPadding.left, false, mIsScrap[0]);
        return view;
    }

    public boolean getItemsCanFocus() {
        return mItemsCanFocus;
    }

    public void setItemsCanFocus(boolean itemsCanFocus) {
        mItemsCanFocus = itemsCanFocus;
        if (!itemsCanFocus) {
            setDescendantFocusability(ViewGroup.FOCUS_BLOCK_DESCENDANTS);
        }
    }

    @Override
    public boolean isOpaque() {
        return (mCachingStarted && mIsCacheColorOpaque && mDividerIsOpaque) || super.isOpaque();
    }

    @Override
    public void setCacheColorHint(int color) {
        final boolean opaque = (color >>> 24) == 0xFF;
        mIsCacheColorOpaque = opaque;
        if (opaque) {
            if (mDividerPaint == null) {
                mDividerPaint = new Paint();
            }
            mDividerPaint.setColor(color);
        }
        super.setCacheColorHint(color);
    }

    void drawOverscrollHeader(Canvas canvas, Drawable drawable, Rect bounds) {
        final int height = drawable.getMinimumHeight();

        canvas.save();
        canvas.clipRect(bounds);

        final int span = bounds.bottom - bounds.top;
        if (span < height) {
            bounds.top = bounds.bottom - height;
        }

        drawable.setBounds(bounds);
        drawable.draw(canvas);

        canvas.restore();
    }

    void drawOverscrollFooter(Canvas canvas, Drawable drawable, Rect bounds) {
        final int height = drawable.getMinimumHeight();

        canvas.save();
        canvas.clipRect(bounds);

        final int span = bounds.bottom - bounds.top;
        if (span < height) {
            bounds.bottom = bounds.top + height;
        }

        drawable.setBounds(bounds);
        drawable.draw(canvas);

        canvas.restore();
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {
        final int dividerHeight = mDividerHeight;
        final Drawable overscrollHeader = mOverScrollHeader;
        final Drawable overscrollFooter = mOverScrollFooter;
        final boolean drawOverscrollHeader = overscrollHeader != null;
        final boolean drawOverscrollFooter = overscrollFooter != null;
        final boolean drawDividers = dividerHeight > 0 && mDivider != null;

        if (drawDividers || drawOverscrollHeader || drawOverscrollFooter) {

            final Rect bounds = mTempRect;
            bounds.left = getPaddingLeft();
            bounds.right = getRight() - getLeft() - getPaddingRight();

            final int count = getChildCount();
            final int headerCount = mHeaderViewInfos.size();
            final int itemCount = mItemCount;
            final int footerLimit = itemCount - mFooterViewInfos.size() - 1;
            final boolean headerDividers = mHeaderDividersEnabled;
            final boolean footerDividers = mFooterDividersEnabled;
            final int first = mFirstPosition;
            final boolean areAllItemsSelectable = mAreAllItemsSelectable;
            final ListAdapter adapter = mAdapter;
            final boolean fillForMissingDividers = drawDividers && isOpaque() && !super.isOpaque();

            if (fillForMissingDividers && mDividerPaint == null && mIsCacheColorOpaque) {
                mDividerPaint = new Paint();
                mDividerPaint.setColor(getCacheColorHint());
            }
            final Paint paint = mDividerPaint;

            final int listBottom = getBottom() - getTop() - mListPadding.bottom + getScrollY();
            if (!mStackFromBottom) {
                int bottom = 0;

                final int scrollY = getScrollY();
                if (count > 0 && scrollY < 0) {
                    if (drawOverscrollHeader) {
                        bounds.bottom = 0;
                        bounds.top = scrollY;
                        drawOverscrollHeader(canvas, overscrollHeader, bounds);
                    } else if (drawDividers) {
                        bounds.bottom = 0;
                        bounds.top = -dividerHeight;
                        drawDivider(canvas, bounds, -1);
                    }
                }

                for (int i = 0; i < count; i++) {
                    if ((headerDividers || first + i >= headerCount) && (footerDividers || first + i < footerLimit)) {
                        View child = getChildAt(i);
                        bottom = child.getBottom();

                        if (drawDividers && (bottom < listBottom && !(drawOverscrollFooter && i == count - 1))) {
                            if ((areAllItemsSelectable || (adapter.isEnabled(first + i) && (i == count - 1 || adapter.isEnabled(first + i + 1))))) {
                                bounds.top = bottom;
                                bounds.bottom = bottom + dividerHeight;
                                drawDivider(canvas, bounds, i);
                            } else if (fillForMissingDividers) {
                                bounds.top = bottom;
                                bounds.bottom = bottom + dividerHeight;
                                canvas.drawRect(bounds, paint);
                            }
                        }
                    }
                }

                final int overFooterBottom = getBottom() + getScrollY();
                if (drawOverscrollFooter && first + count == itemCount && overFooterBottom > bottom) {
                    bounds.top = bottom;
                    bounds.bottom = overFooterBottom;
                    drawOverscrollFooter(canvas, overscrollFooter, bounds);
                }
            } else {
                int top;
                int listTop = mListPadding.top;

                final int scrollY = getScrollY();

                if (count > 0 && drawOverscrollHeader) {
                    bounds.top = scrollY;
                    bounds.bottom = getChildAt(0).getTop();
                    drawOverscrollHeader(canvas, overscrollHeader, bounds);
                }

                final int start = drawOverscrollHeader ? 1 : 0;
                for (int i = start; i < count; i++) {
                    if ((headerDividers || first + i >= headerCount) && (footerDividers || first + i < footerLimit)) {
                        View child = getChildAt(i);
                        top = child.getTop();

                        if (drawDividers && top > listTop) {
                            if ((areAllItemsSelectable || (adapter.isEnabled(first + i) && (i == count - 1 || adapter.isEnabled(first + i + 1))))) {
                                bounds.top = top - dividerHeight;
                                bounds.bottom = top;
                                drawDivider(canvas, bounds, i - 1);
                            } else if (fillForMissingDividers) {
                                bounds.top = top - dividerHeight;
                                bounds.bottom = top;
                                canvas.drawRect(bounds, paint);
                            }
                        }
                    }
                }

                if (count > 0 && scrollY > 0) {
                    if (drawOverscrollFooter) {
                        final int absListBottom = getBottom();
                        bounds.top = absListBottom;
                        bounds.bottom = absListBottom + scrollY;
                        drawOverscrollFooter(canvas, overscrollFooter, bounds);
                    } else if (drawDividers) {
                        bounds.top = listBottom;
                        bounds.bottom = listBottom + dividerHeight;
                        drawDivider(canvas, bounds, -1);
                    }
                }
            }
        }

        super.dispatchDraw(canvas);
    }

    void drawDivider(Canvas canvas, Rect bounds, int childIndex) {
        final Drawable divider = mDivider;
        final boolean clipDivider = mClipDivider;

        if (!clipDivider) {
            divider.setBounds(bounds);
        } else {
            canvas.save();
            canvas.clipRect(bounds);
        }

        divider.draw(canvas);

        if (clipDivider) {
            canvas.restore();
        }
    }

    public Drawable getDivider() {
        return mDivider;
    }

    public void setDivider(Drawable divider) {
        if (divider != null) {
            mDividerHeight = divider.getIntrinsicHeight();
            mClipDivider = divider instanceof ColorDrawable;
        } else {
            mDividerHeight = 0;
            mClipDivider = false;
        }
        mDivider = divider;
        mDividerIsOpaque = divider == null || divider.getOpacity() == PixelFormat.OPAQUE;
        requestLayoutIfNecessary();
    }

    public int getDividerHeight() {
        return mDividerHeight;
    }

    public void setDividerHeight(int height) {
        mDividerHeight = height;
        requestLayoutIfNecessary();
    }

    public void setHeaderDividersEnabled(boolean headerDividersEnabled) {
        mHeaderDividersEnabled = headerDividersEnabled;
        invalidate();
    }

    public void setFooterDividersEnabled(boolean footerDividersEnabled) {
        mFooterDividersEnabled = footerDividersEnabled;
        invalidate();
    }

    public Drawable getOverscrollHeader() {
        return mOverScrollHeader;
    }

    public void setOverscrollHeader(Drawable header) {
        mOverScrollHeader = header;
        if (getScrollY() < 0) {
            invalidate();
        }
    }

    public Drawable getOverscrollFooter() {
        return mOverScrollFooter;
    }

    public void setOverscrollFooter(Drawable footer) {
        mOverScrollFooter = footer;
        invalidate();
    }

    @Override
    protected void onFocusChanged(boolean gainFocus, int direction, Rect previouslyFocusedRect) {
        super.onFocusChanged(gainFocus, direction, previouslyFocusedRect);

        int closetChildIndex = -1;
        if (gainFocus && previouslyFocusedRect != null) {
            previouslyFocusedRect.offset(getScrollX(), getScrollY());

            final ListAdapter adapter = mAdapter;

            if (adapter.getCount() < getChildCount() + mFirstPosition) {
                mLayoutMode = LAYOUT_NORMAL;
                layoutChildren();
            }

            Rect otherRect = mTempRect;
            int minDistance = Integer.MAX_VALUE;
            final int childCount = getChildCount();
            final int firstPosition = mFirstPosition;

            for (int i = 0; i < childCount; i++) {
                if (!adapter.isEnabled(firstPosition + i)) {
                    continue;
                }

                View other = getChildAt(i);
                other.getDrawingRect(otherRect);
                offsetDescendantRectToMyCoords(other, otherRect);
                int distance = getDistance(previouslyFocusedRect, otherRect, direction);

                if (distance < minDistance) {
                    minDistance = distance;
                    closetChildIndex = i;
                }
            }
        }

        if (closetChildIndex >= 0) {
            setSelection(closetChildIndex + mFirstPosition);
        } else {
            requestLayout();
        }
    }

    @Override
    protected void onFinishInflate() {
        super.onFinishInflate();

        int count = getChildCount();
        if (count > 0) {
            for (int i = 0; i < count; ++i) {
                addHeaderView(getChildAt(i));
            }
            removeAllViews();
        }
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        if (mItemsCanFocus && ev.getAction() == MotionEvent.ACTION_DOWN && ev.getEdgeFlags() != 0) {
            return false;
        }
        return super.onTouchEvent(ev);
    }

    @Override
    public boolean performItemClick(View view, int position, long id) {
        boolean handled = false;

        handled |= super.performItemClick(view, position, id);

        return handled;
    }

    public void setItemChecked(int position, boolean value) {
    }

    public boolean isItemChecked(int position) {
        return false;
    }

    public int getCheckedItemPosition() {
        return INVALID_POSITION;
    }

    public SparseBooleanArray getCheckedItemPositions() {
        return null;
    }

    @Deprecated
    public long[] getCheckItemIds() {
        // Use new behavior that correctly handles stable ID mapping.
        if (mAdapter != null && mAdapter.hasStableIds()) {
            return getCheckedItemIds();
        }

        return new long[0];
    }

    public long[] getCheckedItemIds() {
        return new long[0];
    }

    public void clearChoices() {
    }

    public class FixedViewInfo {

        public View view;
        public Object data;
        public boolean isSelectable;
    }

}
