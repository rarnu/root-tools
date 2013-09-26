package com.rarnu.devlib.component.tools;

import android.graphics.Point;
import android.view.*;
import android.widget.AdapterView;
import com.rarnu.devlib.component.DragListView;

public class DragController extends FloatViewManager implements View.OnTouchListener, GestureDetector.OnGestureListener {

    public static final int ON_DOWN = 0;
    public static final int ON_DRAG = 1;
    public static final int ON_LONG_PRESS = 2;
    public static final int CLICK_REMOVE = 0;
    public static final int FLING_REMOVE = 1;
    public static final int MISS = -1;
    private int mDragInitMode = ON_DOWN;
    private boolean mSortEnabled = true;
    private int mRemoveMode;
    private boolean mRemoveEnabled = false;
    private boolean mIsRemoving = false;
    private GestureDetector mDetector;
    private GestureDetector mFlingRemoveDetector;
    private int mTouchSlop;
    private int mHitPos = MISS;
    private int mFlingHitPos = MISS;
    private int mClickRemoveHitPos = MISS;
    private int[] mTempLoc = new int[2];
    private int mItemX;
    private int mItemY;
    private int mCurrX;
    private int mCurrY;
    private boolean mDragging = false;
    private float mFlingSpeed = 500f;
    private int mDragHandleId;
    private int mClickRemoveId;
    private int mFlingHandleId;
    private boolean mCanDrag;
    private DragListView mListview;
    private int mPositionX;
    private GestureDetector.OnGestureListener mFlingRemoveListener = new GestureDetector.SimpleOnGestureListener() {
        @Override
        public final boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
            if (mRemoveEnabled && mIsRemoving) {
                int w = mListview.getWidth();
                int minPos = w / 5;
                if (velocityX > mFlingSpeed) {
                    if (mPositionX > -minPos) {
                        mListview.stopDragWithVelocity(true, velocityX);
                    }
                } else if (velocityX < -mFlingSpeed) {
                    if (mPositionX < minPos) {
                        mListview.stopDragWithVelocity(true, velocityX);
                    }
                }
                mIsRemoving = false;
            }
            return false;
        }
    };

    public DragController(DragListView listview) {
        this(listview, 0, ON_DOWN, FLING_REMOVE);
    }

    public DragController(DragListView listview, int dragHandleId, int dragInitMode, int removeMode) {
        this(listview, dragHandleId, dragInitMode, removeMode, 0);
    }

    public DragController(DragListView listview, int dragHandleId, int dragInitMode, int removeMode, int clickRemoveId) {
        this(listview, dragHandleId, dragInitMode, removeMode, clickRemoveId, 0);
    }

    public DragController(DragListView listview, int dragHandleId, int dragInitMode, int removeMode, int clickRemoveId, int flingHandleId) {
        super(listview);
        mListview = listview;
        mDetector = new GestureDetector(listview.getContext(), this);
        mFlingRemoveDetector = new GestureDetector(listview.getContext(), mFlingRemoveListener);
        mFlingRemoveDetector.setIsLongpressEnabled(false);
        mTouchSlop = ViewConfiguration.get(listview.getContext()).getScaledTouchSlop();
        mDragHandleId = dragHandleId;
        mClickRemoveId = clickRemoveId;
        mFlingHandleId = flingHandleId;
        setRemoveMode(removeMode);
        setDragInitMode(dragInitMode);
    }

    public int getTouchSlop() {
        return mTouchSlop;
    }

    public void setTouchSlop(int touchSlop) {
        mTouchSlop = touchSlop;
    }

    public float getFlingSpeed() {
        return mFlingSpeed;
    }

    public void setFlingSpeed(float flingSpeed) {
        mFlingSpeed = flingSpeed;
    }

    public int getDragInitMode() {
        return mDragInitMode;
    }

    public void setDragInitMode(int mode) {
        mDragInitMode = mode;
    }

    public boolean isSortEnabled() {
        return mSortEnabled;
    }

    public void setSortEnabled(boolean enabled) {
        mSortEnabled = enabled;
    }

    public int getRemoveMode() {
        return mRemoveMode;
    }

    public void setRemoveMode(int mode) {
        mRemoveMode = mode;
    }

    public boolean isRemoveEnabled() {
        return mRemoveEnabled;
    }

    public void setRemoveEnabled(boolean enabled) {
        mRemoveEnabled = enabled;
    }

    public void setDragHandleId(int id) {
        mDragHandleId = id;
    }

    public void setFlingHandleId(int id) {
        mFlingHandleId = id;
    }

    public void setClickRemoveId(int id) {
        mClickRemoveId = id;
    }

    public boolean startDrag(int position, int deltaX, int deltaY) {

        int dragFlags = 0;
        if (mSortEnabled && !mIsRemoving) {
            dragFlags |= DragListView.DRAG_POS_Y | DragListView.DRAG_NEG_Y;
        }
        if (mRemoveEnabled && mIsRemoving) {
            dragFlags |= DragListView.DRAG_POS_X;
            dragFlags |= DragListView.DRAG_NEG_X;
        }

        mDragging = mListview.startDrag(position - mListview.getHeaderViewsCount(), dragFlags, deltaX, deltaY);
        return mDragging;
    }

    @Override
    public boolean onTouch(View v, MotionEvent ev) {
        if (!mListview.isDragEnabled() || mListview.listViewIntercepted()) {
            return false;
        }

        mDetector.onTouchEvent(ev);
        if (mRemoveEnabled && mDragging && mRemoveMode == FLING_REMOVE) {
            mFlingRemoveDetector.onTouchEvent(ev);
        }

        int action = ev.getAction() & MotionEvent.ACTION_MASK;
        switch (action) {
            case MotionEvent.ACTION_DOWN:
                mCurrX = (int) ev.getX();
                mCurrY = (int) ev.getY();
                break;
            case MotionEvent.ACTION_UP:
                if (mRemoveEnabled && mIsRemoving) {
                    int x = mPositionX >= 0 ? mPositionX : -mPositionX;
                    int removePoint = mListview.getWidth() / 2;
                    if (x > removePoint) {
                        mListview.stopDragWithVelocity(true, 0);
                    }
                }
            case MotionEvent.ACTION_CANCEL:
                mIsRemoving = false;
                mDragging = false;
                break;
        }

        return false;
    }

    @Override
    public void onDragFloatView(View floatView, Point position, Point touch) {

        if (mRemoveEnabled && mIsRemoving) {
            mPositionX = position.x;
        }
    }

    public int startDragPosition(MotionEvent ev) {
        return dragHandleHitPosition(ev);
    }

    public int startFlingPosition(MotionEvent ev) {
        return mRemoveMode == FLING_REMOVE ? flingHandleHitPosition(ev) : MISS;
    }

    public int dragHandleHitPosition(MotionEvent ev) {
        return viewIdHitPosition(ev, mDragHandleId);
    }

    public int flingHandleHitPosition(MotionEvent ev) {
        return viewIdHitPosition(ev, mFlingHandleId);
    }

    public int viewIdHitPosition(MotionEvent ev, int id) {
        final int x = (int) ev.getX();
        final int y = (int) ev.getY();

        int touchPos = mListview.pointToPosition(x, y);

        final int numHeaders = mListview.getHeaderViewsCount();
        final int numFooters = mListview.getFooterViewsCount();
        final int count = mListview.getCount();

        if (touchPos != AdapterView.INVALID_POSITION && touchPos >= numHeaders && touchPos < (count - numFooters)) {
            final View item = mListview.getChildAt(touchPos
                    - mListview.getFirstVisiblePosition());
            final int rawX = (int) ev.getRawX();
            final int rawY = (int) ev.getRawY();

            View dragBox = id == 0 ? item : (View) item.findViewById(id);
            if (dragBox != null) {
                dragBox.getLocationOnScreen(mTempLoc);

                if (rawX > mTempLoc[0] && rawY > mTempLoc[1] && rawX < mTempLoc[0] + dragBox.getWidth() && rawY < mTempLoc[1] + dragBox.getHeight()) {

                    mItemX = item.getLeft();
                    mItemY = item.getTop();

                    return touchPos;
                }
            }
        }

        return MISS;
    }

    @Override
    public boolean onDown(MotionEvent ev) {
        if (mRemoveEnabled && mRemoveMode == CLICK_REMOVE) {
            mClickRemoveHitPos = viewIdHitPosition(ev, mClickRemoveId);
        }

        mHitPos = startDragPosition(ev);
        if (mHitPos != MISS && mDragInitMode == ON_DOWN) {
            startDrag(mHitPos, (int) ev.getX() - mItemX, (int) ev.getY() - mItemY);
        }

        mIsRemoving = false;
        mCanDrag = true;
        mPositionX = 0;
        mFlingHitPos = startFlingPosition(ev);

        return true;
    }

    @Override
    public boolean onScroll(MotionEvent e1, MotionEvent e2, float distanceX, float distanceY) {

        final int x1 = (int) e1.getX();
        final int y1 = (int) e1.getY();
        final int x2 = (int) e2.getX();
        final int y2 = (int) e2.getY();
        final int deltaX = x2 - mItemX;
        final int deltaY = y2 - mItemY;

        if (mCanDrag && !mDragging && (mHitPos != MISS || mFlingHitPos != MISS)) {
            if (mHitPos != MISS) {
                if (mDragInitMode == ON_DRAG && Math.abs(y2 - y1) > mTouchSlop && mSortEnabled) {
                    startDrag(mHitPos, deltaX, deltaY);
                } else if (mDragInitMode != ON_DOWN && Math.abs(x2 - x1) > mTouchSlop && mRemoveEnabled) {
                    mIsRemoving = true;
                    startDrag(mFlingHitPos, deltaX, deltaY);
                }
            } else if (mFlingHitPos != MISS) {
                if (Math.abs(x2 - x1) > mTouchSlop && mRemoveEnabled) {
                    mIsRemoving = true;
                    startDrag(mFlingHitPos, deltaX, deltaY);
                } else if (Math.abs(y2 - y1) > mTouchSlop) {
                    mCanDrag = false;
                }
            }
        }

        return false;
    }

    @Override
    public void onLongPress(MotionEvent e) {
        if (mHitPos != MISS && mDragInitMode == ON_LONG_PRESS) {
            mListview.performHapticFeedback(HapticFeedbackConstants.LONG_PRESS);
            startDrag(mHitPos, mCurrX - mItemX, mCurrY - mItemY);
        }
    }

    @Override
    public final boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
        return false;
    }

    @Override
    public boolean onSingleTapUp(MotionEvent ev) {
        if (mRemoveEnabled && mRemoveMode == CLICK_REMOVE) {
            if (mClickRemoveHitPos != MISS) {
                mListview.removeItem(mClickRemoveHitPos - mListview.getHeaderViewsCount());
            }
        }
        return true;
    }

    @Override
    public void onShowPress(MotionEvent ev) {

    }

}
