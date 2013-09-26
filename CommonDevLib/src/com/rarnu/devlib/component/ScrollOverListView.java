package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.widget.ListView;
import com.rarnu.devlib.component.intf.OnScrollOverListener;

public class ScrollOverListView extends ListView {

    private int mLastY;
    private int mTopPosition;
    private int mBottomPosition;
    private OnScrollOverListener mOnScrollOverListener = new OnScrollOverListener() {

        @Override
        public boolean onListViewTopAndPullDown(int delta) {
            return false;
        }

        @Override
        public boolean onListViewBottomAndPullUp(int delta) {
            return false;
        }

        @Override
        public boolean onMotionDown(MotionEvent ev) {
            return false;
        }

        @Override
        public boolean onMotionMove(MotionEvent ev, int delta) {
            return false;
        }

        @Override
        public boolean onMotionUp(MotionEvent ev) {
            return false;
        }

    };

    public ScrollOverListView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public ScrollOverListView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public ScrollOverListView(Context context) {
        super(context);
        init();
    }

    private void init() {
        mTopPosition = 0;
        mBottomPosition = 0;
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        final int action = ev.getAction();
        final int y = (int) ev.getRawY();

        switch (action) {
            case MotionEvent.ACTION_DOWN: {
                mLastY = y;
                final boolean isHandled = mOnScrollOverListener.onMotionDown(ev);
                if (isHandled) {
                    mLastY = y;
                    return isHandled;
                }
                break;
            }

            case MotionEvent.ACTION_MOVE: {
                final int childCount = getChildCount();
                if (childCount == 0) {
                    return super.onTouchEvent(ev);
                }
                final int itemCount = getAdapter().getCount() - mBottomPosition;

                final int deltaY = y - mLastY;

                final int firstTop = getChildAt(0).getTop();
                final int listPadding = getListPaddingTop();

                final int lastBottom = getChildAt(childCount - 1).getBottom();
                final int end = getHeight() - getPaddingBottom();

                final int firstVisiblePosition = getFirstVisiblePosition();

                final boolean isHandleMotionMove = mOnScrollOverListener.onMotionMove(ev, deltaY);

                if (isHandleMotionMove) {
                    mLastY = y;
                    return true;
                }

                if (firstVisiblePosition <= mTopPosition && firstTop >= listPadding && deltaY > 0) {
                    final boolean isHandleOnListViewTopAndPullDown;
                    isHandleOnListViewTopAndPullDown = mOnScrollOverListener.onListViewTopAndPullDown(deltaY);
                    if (isHandleOnListViewTopAndPullDown) {
                        mLastY = y;
                        return true;
                    }
                }

                if (firstVisiblePosition + childCount >= itemCount && lastBottom <= end && deltaY < 0) {
                    final boolean isHandleOnListViewBottomAndPullDown;
                    isHandleOnListViewBottomAndPullDown = mOnScrollOverListener.onListViewBottomAndPullUp(deltaY);
                    if (isHandleOnListViewBottomAndPullDown) {
                        mLastY = y;
                        return true;
                    }
                }
                break;
            }

            case MotionEvent.ACTION_UP: {
                final boolean isHandlerMotionUp = mOnScrollOverListener.onMotionUp(ev);
                if (isHandlerMotionUp) {
                    mLastY = y;
                    return true;
                }
                break;
            }
        }

        mLastY = y;
        return super.onTouchEvent(ev);
    }

    public void setTopPosition(int index) {
        if (getAdapter() == null) {
            throw new NullPointerException("You must set adapter before setTopPosition!");
        }
        if (index < 0) {
            throw new IllegalArgumentException("Top position must > 0");
        }
        mTopPosition = index;
    }

    public void setBottomPosition(int index) {
        if (getAdapter() == null) {
            throw new NullPointerException("You must set adapter before setBottonPosition!");
        }
        if (index < 0) {
            throw new IllegalArgumentException("Bottom position must > 0");
        }
        mBottomPosition = index;
    }

    public void setOnScrollOverListener(OnScrollOverListener onScrollOverListener) {
        mOnScrollOverListener = onScrollOverListener;
    }

}
