package com.rarnu.devlib.base;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.*;
import android.widget.AbsListView.OnScrollListener;
import android.widget.AdapterView.OnItemClickListener;
import com.rarnu.devlib.R;
import com.rarnu.devlib.component.IndicatorLayout;
import com.rarnu.devlib.component.tools.EmptyViewMethodAccessor;

public abstract class PullToRefreshAdapterViewBase<T extends AbsListView> extends PullToRefreshBase<T> implements
        OnScrollListener {

    private static FrameLayout.LayoutParams convertEmptyViewLayoutParams(ViewGroup.LayoutParams lp) {
        FrameLayout.LayoutParams newLp = null;

        if (null != lp) {
            newLp = new FrameLayout.LayoutParams(lp);

            if (lp instanceof LinearLayout.LayoutParams) {
                newLp.gravity = ((LinearLayout.LayoutParams) lp).gravity;
            } else {
                newLp.gravity = Gravity.CENTER;
            }
        }

        return newLp;
    }

    private boolean mLastItemVisible;
    private OnScrollListener mOnScrollListener;
    private OnLastItemVisibleListener mOnLastItemVisibleListener;
    private View mEmptyView;

    private IndicatorLayout mIndicatorIvTop;
    private IndicatorLayout mIndicatorIvBottom;

    private boolean mShowIndicator;
    private boolean mScrollEmptyView = true;

    public PullToRefreshAdapterViewBase(Context context) {
        super(context);
        mRefreshableView.setOnScrollListener(this);
    }

    public PullToRefreshAdapterViewBase(Context context, AttributeSet attrs) {
        super(context, attrs);
        mRefreshableView.setOnScrollListener(this);
    }

    public PullToRefreshAdapterViewBase(Context context, Mode mode) {
        super(context, mode);
        mRefreshableView.setOnScrollListener(this);
    }

    public PullToRefreshAdapterViewBase(Context context, Mode mode, AnimationStyle animStyle) {
        super(context, mode, animStyle);
        mRefreshableView.setOnScrollListener(this);
    }

    public boolean getShowIndicator() {
        return mShowIndicator;
    }

    public final void onScroll(final AbsListView view, final int firstVisibleItem, final int visibleItemCount,
                               final int totalItemCount) {

        if (null != mOnLastItemVisibleListener) {
            mLastItemVisible = (totalItemCount > 0) && (firstVisibleItem + visibleItemCount >= totalItemCount - 1);
        }

        if (getShowIndicatorInternal()) {
            updateIndicatorViewsVisibility();
        }

        if (null != mOnScrollListener) {
            mOnScrollListener.onScroll(view, firstVisibleItem, visibleItemCount, totalItemCount);
        }
    }

    public final void onScrollStateChanged(final AbsListView view, final int state) {

        if (state == OnScrollListener.SCROLL_STATE_IDLE && null != mOnLastItemVisibleListener && mLastItemVisible) {
            mOnLastItemVisibleListener.onLastItemVisible();
        }

        if (null != mOnScrollListener) {
            mOnScrollListener.onScrollStateChanged(view, state);
        }
    }

    public void setAdapter(ListAdapter adapter) {
        ((AdapterView<ListAdapter>) mRefreshableView).setAdapter(adapter);
    }

    public final void setEmptyView(View newEmptyView) {
        FrameLayout refreshableViewWrapper = getRefreshableViewWrapper();

        if (null != newEmptyView) {
            newEmptyView.setClickable(true);

            ViewParent newEmptyViewParent = newEmptyView.getParent();
            if (null != newEmptyViewParent && newEmptyViewParent instanceof ViewGroup) {
                ((ViewGroup) newEmptyViewParent).removeView(newEmptyView);
            }

            FrameLayout.LayoutParams lp = convertEmptyViewLayoutParams(newEmptyView.getLayoutParams());
            if (null != lp) {
                refreshableViewWrapper.addView(newEmptyView, lp);
            } else {
                refreshableViewWrapper.addView(newEmptyView);
            }
        }

        if (mRefreshableView instanceof EmptyViewMethodAccessor) {
            ((EmptyViewMethodAccessor) mRefreshableView).setEmptyViewInternal(newEmptyView);
        } else {
            mRefreshableView.setEmptyView(newEmptyView);
        }
        mEmptyView = newEmptyView;
    }

    public void setOnItemClickListener(OnItemClickListener listener) {
        mRefreshableView.setOnItemClickListener(listener);
    }

    public final void setOnLastItemVisibleListener(OnLastItemVisibleListener listener) {
        mOnLastItemVisibleListener = listener;
    }

    public final void setOnScrollListener(OnScrollListener listener) {
        mOnScrollListener = listener;
    }

    public final void setScrollEmptyView(boolean doScroll) {
        mScrollEmptyView = doScroll;
    }

    public void setShowIndicator(boolean showIndicator) {
        mShowIndicator = showIndicator;

        if (getShowIndicatorInternal()) {
            addIndicatorViews();
        } else {
            removeIndicatorViews();
        }
    }

    @Override
    protected void onPullToRefresh() {
        super.onPullToRefresh();

        if (getShowIndicatorInternal()) {
            switch (getCurrentMode()) {
                case PULL_FROM_END:
                    mIndicatorIvBottom.pullToRefresh();
                    break;
                case PULL_FROM_START:
                    mIndicatorIvTop.pullToRefresh();
                    break;
                default:
                    break;
            }
        }
    }

    protected void onRefreshing(boolean doScroll) {
        super.onRefreshing(doScroll);

        if (getShowIndicatorInternal()) {
            updateIndicatorViewsVisibility();
        }
    }

    @Override
    protected void onReleaseToRefresh() {
        super.onReleaseToRefresh();

        if (getShowIndicatorInternal()) {
            switch (getCurrentMode()) {
                case PULL_FROM_END:
                    mIndicatorIvBottom.releaseToRefresh();
                    break;
                case PULL_FROM_START:
                    mIndicatorIvTop.releaseToRefresh();
                    break;
                default:
                    break;
            }
        }
    }

    @Override
    protected void onReset() {
        super.onReset();

        if (getShowIndicatorInternal()) {
            updateIndicatorViewsVisibility();
        }
    }

    @Override
    protected void handleStyledAttributes(TypedArray a) {
        mShowIndicator = a.getBoolean(R.styleable.PullToRefresh_ptrShowIndicator, !isPullToRefreshOverScrollEnabled());
    }

    protected boolean isReadyForPullStart() {
        return isFirstItemVisible();
    }

    protected boolean isReadyForPullEnd() {
        return isLastItemVisible();
    }

    @Override
    protected void onScrollChanged(int l, int t, int oldl, int oldt) {
        super.onScrollChanged(l, t, oldl, oldt);
        if (null != mEmptyView && !mScrollEmptyView) {
            mEmptyView.scrollTo(-l, -t);
        }
    }

    @Override
    protected void updateUIForMode() {
        super.updateUIForMode();

        if (getShowIndicatorInternal()) {
            addIndicatorViews();
        } else {
            removeIndicatorViews();
        }
    }

    private void addIndicatorViews() {
        Mode mode = getMode();
        FrameLayout refreshableViewWrapper = getRefreshableViewWrapper();

        if (mode.showHeaderLoadingLayout() && null == mIndicatorIvTop) {

            mIndicatorIvTop = new IndicatorLayout(getContext(), Mode.PULL_FROM_START);
            FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT,
                    ViewGroup.LayoutParams.WRAP_CONTENT);
            params.rightMargin = getResources().getDimensionPixelSize(R.dimen.indicator_right_padding);
            params.gravity = Gravity.TOP | Gravity.RIGHT;
            refreshableViewWrapper.addView(mIndicatorIvTop, params);

        } else if (!mode.showHeaderLoadingLayout() && null != mIndicatorIvTop) {
            refreshableViewWrapper.removeView(mIndicatorIvTop);
            mIndicatorIvTop = null;
        }

        if (mode.showFooterLoadingLayout() && null == mIndicatorIvBottom) {
            mIndicatorIvBottom = new IndicatorLayout(getContext(), Mode.PULL_FROM_END);
            FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT,
                    ViewGroup.LayoutParams.WRAP_CONTENT);
            params.rightMargin = getResources().getDimensionPixelSize(R.dimen.indicator_right_padding);
            params.gravity = Gravity.BOTTOM | Gravity.RIGHT;
            refreshableViewWrapper.addView(mIndicatorIvBottom, params);

        } else if (!mode.showFooterLoadingLayout() && null != mIndicatorIvBottom) {
            refreshableViewWrapper.removeView(mIndicatorIvBottom);
            mIndicatorIvBottom = null;
        }
    }

    private boolean getShowIndicatorInternal() {
        return mShowIndicator && isPullToRefreshEnabled();
    }

    private boolean isFirstItemVisible() {
        final Adapter adapter = mRefreshableView.getAdapter();

        if (null == adapter || adapter.isEmpty()) {
            return true;

        } else {

            if (mRefreshableView.getFirstVisiblePosition() <= 1) {
                final View firstVisibleChild = mRefreshableView.getChildAt(0);
                if (firstVisibleChild != null) {
                    return firstVisibleChild.getTop() >= mRefreshableView.getTop();
                }
            }
        }

        return false;
    }

    private boolean isLastItemVisible() {
        final Adapter adapter = mRefreshableView.getAdapter();

        if (null == adapter || adapter.isEmpty()) {

            return true;
        } else {
            final int lastItemPosition = mRefreshableView.getCount() - 1;
            final int lastVisiblePosition = mRefreshableView.getLastVisiblePosition();

            if (lastVisiblePosition >= lastItemPosition - 1) {
                final int childIndex = lastVisiblePosition - mRefreshableView.getFirstVisiblePosition();
                final View lastVisibleChild = mRefreshableView.getChildAt(childIndex);
                if (lastVisibleChild != null) {
                    return lastVisibleChild.getBottom() <= mRefreshableView.getBottom();
                }
            }
        }

        return false;
    }

    private void removeIndicatorViews() {
        if (null != mIndicatorIvTop) {
            getRefreshableViewWrapper().removeView(mIndicatorIvTop);
            mIndicatorIvTop = null;
        }

        if (null != mIndicatorIvBottom) {
            getRefreshableViewWrapper().removeView(mIndicatorIvBottom);
            mIndicatorIvBottom = null;
        }
    }

    private void updateIndicatorViewsVisibility() {
        if (null != mIndicatorIvTop) {
            if (!isRefreshing() && isReadyForPullStart()) {
                if (!mIndicatorIvTop.isVisible()) {
                    mIndicatorIvTop.show();
                }
            } else {
                if (mIndicatorIvTop.isVisible()) {
                    mIndicatorIvTop.hide();
                }
            }
        }

        if (null != mIndicatorIvBottom) {
            if (!isRefreshing() && isReadyForPullEnd()) {
                if (!mIndicatorIvBottom.isVisible()) {
                    mIndicatorIvBottom.show();
                }
            } else {
                if (mIndicatorIvBottom.isVisible()) {
                    mIndicatorIvBottom.hide();
                }
            }
        }
    }
}
