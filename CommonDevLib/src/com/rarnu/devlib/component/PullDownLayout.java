package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.LinearInterpolator;
import android.view.animation.RotateAnimation;
import android.widget.*;
import com.rarnu.devlib.R;

public class PullDownLayout extends LinearLayout {

    public static final int WHAT_DID_REFRESH = 1;
    private static final int TAP_TO_REFRESH = 1;
    private static final int PULL_TO_REFRESH = 2;
    private static final int RELEASE_TO_REFRESH = 3;
    private static final int REFRESHING = 4;
    public int mRefreshState;
    public Scroller scroller;
    public ScrollView sv;
    public int nowpull = -1;
    private View refreshView;
    private ImageView refreshIndicatorView;
    private int refreshTargetTop = -105;
    private ProgressBar bar;
    private TextView downTextView;
    private RefreshListener refreshListener;
    private int lastY;
    private RotateAnimation mFlipAnimation;
    private RotateAnimation mReverseFlipAnimation;
    private Context mContext;

    public PullDownLayout(Context context) {
        super(context);
        mContext = context;
    }

    public PullDownLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        init();
    }

    private void init() {

        mFlipAnimation = new RotateAnimation(0, -180, RotateAnimation.RELATIVE_TO_SELF, 0.5f, RotateAnimation.RELATIVE_TO_SELF, 0.5f);
        mFlipAnimation.setInterpolator(new LinearInterpolator());
        mFlipAnimation.setDuration(250);
        mFlipAnimation.setFillAfter(true);

        mReverseFlipAnimation = new RotateAnimation(-180, 0, RotateAnimation.RELATIVE_TO_SELF, 0.5f, RotateAnimation.RELATIVE_TO_SELF, 0.5f);
        mReverseFlipAnimation.setInterpolator(new LinearInterpolator());
        mReverseFlipAnimation.setDuration(250);
        mReverseFlipAnimation.setFillAfter(true);
        scroller = new Scroller(mContext);
        refreshView = LayoutInflater.from(mContext).inflate(R.layout.pulldown_header, null);
        refreshIndicatorView = (ImageView) refreshView.findViewById(R.id.pulldown_header_arrow);
        bar = (ProgressBar) refreshView.findViewById(R.id.pulldown_header_loading);
        downTextView = (TextView) refreshView.findViewById(R.id.pulldown_header_text);

        refreshView.setMinimumHeight(50);
        LayoutParams lp = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, -refreshTargetTop);
        lp.topMargin = refreshTargetTop;
        lp.gravity = Gravity.CENTER;
        addView(refreshView, lp);
        mRefreshState = TAP_TO_REFRESH;

    }

    public boolean onTouchEvent(MotionEvent event) {
        int y = (int) event.getRawY();
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                lastY = y;
                break;
            case MotionEvent.ACTION_MOVE:
                int m = y - lastY;
                doMovement(m);
                lastY = y;
                break;
            case MotionEvent.ACTION_UP:
                fling();
                break;
        }
        return true;
    }

    private void fling() {
        if (nowpull == 0 && mRefreshState != REFRESHING) {
            LinearLayout.LayoutParams lp = (LayoutParams) refreshView.getLayoutParams();
            if (lp.topMargin > 0) {
                refresh();
            } else {
                returnInitState();
            }
        }
    }

    public void onRefresh() {
        if (refreshListener != null) {
            refreshListener.onRefresh();
        }
    }

    private void returnInitState() {
        mRefreshState = TAP_TO_REFRESH;
        LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) this.refreshView.getLayoutParams();
        int i = lp.topMargin;
        scroller.startScroll(0, i, 0, refreshTargetTop);
        invalidate();
    }

    private void refresh() {
        mRefreshState = REFRESHING;
        LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) this.refreshView.getLayoutParams();
        int i = lp.topMargin;
        refreshIndicatorView.setVisibility(View.GONE);
        refreshIndicatorView.setImageDrawable(null);
        bar.setVisibility(View.VISIBLE);

        downTextView.setText(R.string.list_loading);
        scroller.startScroll(0, i, 0, 0 - i);
        invalidate();

        if (refreshListener != null) {
            refreshListener.onRefresh();
        }

    }

    @Override
    public void computeScroll() {
        if (scroller.computeScrollOffset()) {
            int i = this.scroller.getCurrY();
            LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) this.refreshView.getLayoutParams();
            int k = Math.max(i, refreshTargetTop);
            lp.topMargin = k;
            this.refreshView.setLayoutParams(lp);
            this.refreshView.invalidate();
            invalidate();
        }
    }

    public void doMovement(int moveY) {
        LinearLayout.LayoutParams lp = (LayoutParams) refreshView.getLayoutParams();

        if (sv.getScrollY() == 0 && moveY > 0) {
            nowpull = 0;
        }
        if (sv.getChildAt(0).getMeasuredHeight() <= sv.getScrollY() + getHeight() && moveY < 0 && lp.topMargin <= refreshTargetTop) {
            nowpull = 1;
        }

        if (nowpull == 0 && mRefreshState != REFRESHING) {
            float f1 = lp.topMargin;
            float f2 = f1 + moveY * 0.3F;
            int i = (int) f2;
            lp.topMargin = i;
            refreshView.setLayoutParams(lp);
            refreshView.invalidate();
            invalidate();

            downTextView.setVisibility(View.VISIBLE);

            refreshIndicatorView.setVisibility(View.VISIBLE);
            bar.setVisibility(View.GONE);
            if (lp.topMargin > 0 && mRefreshState != RELEASE_TO_REFRESH) {
                downTextView.setText(R.string.list_release_refresh);

                refreshIndicatorView.clearAnimation();
                refreshIndicatorView.startAnimation(mFlipAnimation);
                mRefreshState = RELEASE_TO_REFRESH;
            } else if (lp.topMargin <= 0 && mRefreshState != PULL_TO_REFRESH) {
                downTextView.setText(R.string.list_pull_refresh);

                if (mRefreshState != TAP_TO_REFRESH) {
                    refreshIndicatorView.clearAnimation();
                    refreshIndicatorView.startAnimation(mReverseFlipAnimation);
                }
                mRefreshState = PULL_TO_REFRESH;
            }
        }

    }

    public void setRefreshListener(RefreshListener listener) {
        this.refreshListener = listener;
    }

    public void finishRefresh() {

        if (mRefreshState != TAP_TO_REFRESH) {
            mRefreshState = TAP_TO_REFRESH;
            LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) this.refreshView.getLayoutParams();
            int i = lp.topMargin;
            refreshIndicatorView.setImageResource(R.drawable.z_arrow_down);
            refreshIndicatorView.clearAnimation();
            bar.setVisibility(View.GONE);
            refreshIndicatorView.setVisibility(View.GONE);
            downTextView.setText(R.string.list_pull_refresh);
            scroller.startScroll(0, i, 0, refreshTargetTop);
            invalidate();
        }

    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent e) {
        int action = e.getAction();
        int y = (int) e.getRawY();

        switch (action) {
            case MotionEvent.ACTION_DOWN:
                lastY = y;
                break;

            case MotionEvent.ACTION_MOVE:
                int m = y - lastY;
                if (canScroll(m)) {
                    return true;
                }
                break;
        }
        return false;
    }

    private boolean canScroll(int diff) {
        View childView;
        if (mRefreshState == REFRESHING) {
            return true;
        }
        if (getChildCount() > 1) {
            childView = this.getChildAt(1);
            if (childView instanceof ListView) {
                int top = ((ListView) childView).getChildAt(0).getTop();
                int pad = ((ListView) childView).getListPaddingTop();
                if ((Math.abs(top - pad)) < 3 && ((ListView) childView).getFirstVisiblePosition() == 0) {
                    return true;
                } else {
                    return false;
                }
            } else if (childView instanceof ScrollView) {
                if (((ScrollView) childView).getScrollY() == 0 && diff > 0) {
                    nowpull = 0;
                    return true;
                } else if ((((ScrollView) childView).getChildAt(0).getMeasuredHeight() <= ((ScrollView) childView).getScrollY() + getHeight()) && diff < 0) {
                    nowpull = 1;
                    return true;
                } else {
                    return false;
                }
            }
        }
        return false;
    }

    public interface RefreshListener {
        public void onRefresh();
    }
}
