package com.rarnu.devlib.base.inner;

import android.content.Context;
import android.database.DataSetObserver;
import android.os.Parcelable;
import android.os.SystemClock;
import android.util.AttributeSet;
import android.util.SparseArray;
import android.view.ContextMenu;
import android.view.SoundEffectConstants;
import android.view.View;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityEvent;
import android.widget.Adapter;

public abstract class InnerAdapterView<T extends Adapter> extends ViewGroup {

    public static final int ITEM_VIEW_TYPE_IGNORE = -1;
    public static final int ITEM_VIEW_TYPE_HEADER_OR_FOOTER = -2;
    public static final int INVALID_POSITION = -1;
    public static final long INVALID_ROW_ID = Long.MIN_VALUE;
    static final int SYNC_SELECTED_POSITION = 0;
    static final int SYNC_FIRST_POSITION = 1;
    static final int SYNC_MAX_DURATION_MILLIS = 100;
    int mFirstPosition = 0;
    int mSpecificTop;
    int mSyncPosition;
    long mSyncRowId = INVALID_ROW_ID;
    long mSyncHeight;
    boolean mNeedSync = false;
    int mSyncMode;
    boolean mInLayout = false;
    OnItemSelectedListener mOnItemSelectedListener;
    OnItemClickListener mOnItemClickListener;
    OnItemLongClickListener mOnItemLongClickListener;
    boolean mDataChanged;
    int mItemCount;
    int mOldItemCount;
    int mOldSelectedPosition = INVALID_POSITION;
    long mOldSelectedRowId = INVALID_ROW_ID;
    boolean mBlockLayoutRequests = false;
    private int mLayoutHeight;
    private View mEmptyView;
    private boolean mDesiredFocusableState;
    private boolean mDesiredFocusableInTouchModeState;

    public InnerAdapterView(Context context) {
        super(context);
    }

    public InnerAdapterView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public InnerAdapterView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    public final OnItemClickListener getOnItemClickListener() {
        return mOnItemClickListener;
    }

    public void setOnItemClickListener(OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    public boolean performItemClick(View view, int position, long id) {
        if (mOnItemClickListener != null) {
            playSoundEffect(SoundEffectConstants.CLICK);
            mOnItemClickListener.onItemClick(this, view, position, id);
            return true;
        }

        return false;
    }

    public final OnItemLongClickListener getOnItemLongClickListener() {
        return mOnItemLongClickListener;
    }

    public void setOnItemLongClickListener(OnItemLongClickListener listener) {
        if (!isLongClickable()) {
            setLongClickable(true);
        }
        mOnItemLongClickListener = listener;
    }

    public final OnItemSelectedListener getOnItemSelectedListener() {
        return mOnItemSelectedListener;
    }

    public void setOnItemSelectedListener(OnItemSelectedListener listener) {
        mOnItemSelectedListener = listener;
    }

    public abstract T getAdapter();

    public abstract void setAdapter(T adapter);

    @Override
    public void addView(View child) {
        throw new UnsupportedOperationException("addView(View) is not supported in AdapterView");
    }

    @Override
    public void addView(View child, int index) {
        throw new UnsupportedOperationException("addView(View, int) is not supported in AdapterView");
    }

    @Override
    public void addView(View child, LayoutParams params) {
        throw new UnsupportedOperationException("addView(View, LayoutParams) is not supported in AdapterView");
    }

    @Override
    public void addView(View child, int index, LayoutParams params) {
        throw new UnsupportedOperationException("addView(View, int, LayoutParams) is not supported in AdapterView");
    }

    @Override
    public void removeView(View child) {
        throw new UnsupportedOperationException("removeView(View) is not supported in AdapterView");
    }

    @Override
    public void removeViewAt(int index) {
        throw new UnsupportedOperationException("removeViewAt(int) is not supported in AdapterView");
    }

    @Override
    public void removeAllViews() {
        throw new UnsupportedOperationException("removeAllViews() is not supported in AdapterView");
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        mLayoutHeight = getHeight();
    }

    public int getSelectedItemPosition() {
        return INVALID_POSITION;
    }

    public long getSelectedItemId() {
        return INVALID_ROW_ID;
    }

    public abstract View getSelectedView();

    public Object getSelectedItem() {
        T adapter = getAdapter();
        int selection = getSelectedItemPosition();
        if (adapter != null && adapter.getCount() > 0 && selection >= 0) {
            return adapter.getItem(selection);
        } else {
            return null;
        }
    }

    public int getCount() {
        return mItemCount;
    }

    public int getPositionForView(View view) {
        View listItem = view;
        try {
            View v;
            while (!(v = (View) listItem.getParent()).equals(this)) {
                listItem = v;
            }
        } catch (ClassCastException e) {
            return INVALID_POSITION;
        }

        final int childCount = getChildCount();
        for (int i = 0; i < childCount; i++) {
            if (getChildAt(i).equals(listItem)) {
                return mFirstPosition + i;
            }
        }

        return INVALID_POSITION;
    }

    public int getFirstVisiblePosition() {
        return mFirstPosition;
    }

    public int getLastVisiblePosition() {
        return mFirstPosition + getChildCount() - 1;
    }

    public abstract void setSelection(int position);

    public View getEmptyView() {
        return mEmptyView;
    }

    public void setEmptyView(View emptyView) {
        mEmptyView = emptyView;

        final T adapter = getAdapter();
        final boolean empty = ((adapter == null) || adapter.isEmpty());
        updateEmptyStatus(empty);
    }

    boolean isInFilterMode() {
        return false;
    }

    @Override
    public void setFocusable(boolean focusable) {
        final T adapter = getAdapter();
        final boolean empty = adapter == null || adapter.getCount() == 0;

        mDesiredFocusableState = focusable;
        if (!focusable) {
            mDesiredFocusableInTouchModeState = false;
        }

        super.setFocusable(focusable && (!empty || isInFilterMode()));
    }

    @Override
    public void setFocusableInTouchMode(boolean focusable) {
        final T adapter = getAdapter();
        final boolean empty = adapter == null || adapter.getCount() == 0;

        mDesiredFocusableInTouchModeState = focusable;
        if (focusable) {
            mDesiredFocusableState = true;
        }

        super.setFocusableInTouchMode(focusable && (!empty || isInFilterMode()));
    }

    void checkFocus() {
        final T adapter = getAdapter();
        final boolean empty = adapter == null || adapter.getCount() == 0;
        final boolean focusable = !empty || isInFilterMode();
        super.setFocusableInTouchMode(focusable && mDesiredFocusableInTouchModeState);
        super.setFocusable(focusable && mDesiredFocusableState);
        if (mEmptyView != null) {
            updateEmptyStatus((adapter == null) || adapter.isEmpty());
        }
    }

    private void updateEmptyStatus(boolean empty) {
        if (isInFilterMode()) {
            empty = false;
        }

        if (empty) {
            if (mEmptyView != null) {
                mEmptyView.setVisibility(View.VISIBLE);
                setVisibility(View.GONE);
            } else {
                setVisibility(View.VISIBLE);
            }
            if (mDataChanged) {
                this.onLayout(false, getLeft(), getTop(), getRight(), getBottom());
            }
        } else {
            if (mEmptyView != null)
                mEmptyView.setVisibility(View.GONE);
            setVisibility(View.VISIBLE);
        }
    }

    public Object getItemAtPosition(int position) {
        T adapter = getAdapter();
        return (adapter == null || position < 0) ? null : adapter.getItem(position);
    }

    public long getItemIdAtPosition(int position) {
        T adapter = getAdapter();
        return (adapter == null || position < 0) ? INVALID_ROW_ID : adapter.getItemId(position);
    }

    @Override
    public void setOnClickListener(OnClickListener l) {
        throw new RuntimeException("Don't call setOnClickListener for an AdapterView. You probably want setOnItemClickListener instead");
    }

    @Override
    protected void dispatchSaveInstanceState(SparseArray<Parcelable> container) {
        dispatchFreezeSelfOnly(container);
    }

    @Override
    protected void dispatchRestoreInstanceState(
            SparseArray<Parcelable> container) {
        dispatchThawSelfOnly(container);
    }

    @Override
    public boolean dispatchPopulateAccessibilityEvent(AccessibilityEvent event) {
        boolean populated = false;
        if (event.getEventType() == AccessibilityEvent.TYPE_VIEW_FOCUSED) {
            event.setEventType(AccessibilityEvent.TYPE_VIEW_SELECTED);
        }
        View selectedView = getSelectedView();
        if (selectedView != null) {
            populated = selectedView.dispatchPopulateAccessibilityEvent(event);
        }

        if (!populated) {
            if (selectedView != null) {
                event.setEnabled(selectedView.isEnabled());
            }
            event.setItemCount(getCount());
            event.setCurrentItemIndex(getSelectedItemPosition());
        }

        return populated;
    }

    @Override
    protected boolean canAnimate() {
        return super.canAnimate() && mItemCount > 0;
    }

    void handleDataChanged() {
        final int count = mItemCount;

        if (count > 0) {
            if (mNeedSync) {
                mNeedSync = false;
            }
        }
    }

    int findSyncPosition() {
        int count = mItemCount;

        if (count == 0) {
            return INVALID_POSITION;
        }

        long idToMatch = mSyncRowId;
        int seed = mSyncPosition;
        if (idToMatch == INVALID_ROW_ID) {
            return INVALID_POSITION;
        }

        seed = Math.max(0, seed);
        seed = Math.min(count - 1, seed);

        long endTime = SystemClock.uptimeMillis() + SYNC_MAX_DURATION_MILLIS;

        long rowId;
        int first = seed;
        int last = seed;
        boolean next = false;
        boolean hitFirst;
        boolean hitLast;

        T adapter = getAdapter();
        if (adapter == null) {
            return INVALID_POSITION;
        }

        while (SystemClock.uptimeMillis() <= endTime) {
            rowId = adapter.getItemId(seed);
            if (rowId == idToMatch) {
                return seed;
            }

            hitLast = last == count - 1;
            hitFirst = first == 0;

            if (hitLast && hitFirst) {
                break;
            }

            if (hitFirst || (next && !hitLast)) {
                last++;
                seed = last;
                next = false;
            } else if (hitLast || (!next && !hitFirst)) {
                first--;
                seed = first;
                next = true;
            }

        }

        return INVALID_POSITION;
    }

    int lookForSelectablePosition(int position, boolean lookDown) {
        return position;
    }

    void rememberSyncState() {
        if (getChildCount() > 0) {
            mNeedSync = true;
            mSyncHeight = mLayoutHeight;
            View v = getChildAt(0);
            T adapter = getAdapter();
            if (mFirstPosition >= 0 && mFirstPosition < adapter.getCount()) {
                mSyncRowId = adapter.getItemId(mFirstPosition);
            } else {
                mSyncRowId = NO_ID;
            }
            mSyncPosition = mFirstPosition;
            if (v != null) {
                mSpecificTop = v.getTop();
            }
            mSyncMode = SYNC_FIRST_POSITION;
        }
    }

    public interface OnItemClickListener {
        void onItemClick(InnerAdapterView<?> parent, View view, int position, long id);
    }

    public interface OnItemLongClickListener {
        boolean onItemLongClick(InnerAdapterView<?> parent, View view, int position, long id);
    }

    public interface OnItemSelectedListener {
        void onItemSelected(InnerAdapterView<?> parent, View view, int position, long id);

        void onNothingSelected(InnerAdapterView<?> parent);
    }

    public static class AdapterContextMenuInfo implements ContextMenu.ContextMenuInfo {

        public View targetView;
        public int position;
        public long id;

        public AdapterContextMenuInfo(View targetView, int position, long id) {
            this.targetView = targetView;
            this.position = position;
            this.id = id;
        }
    }

    class AdapterDataSetObserver extends DataSetObserver {

        private Parcelable mInstanceState = null;

        @Override
        public void onChanged() {
            mDataChanged = true;
            mOldItemCount = mItemCount;
            mItemCount = getAdapter().getCount();
            if (InnerAdapterView.this.getAdapter().hasStableIds() && mInstanceState != null && mOldItemCount == 0 && mItemCount > 0) {
                InnerAdapterView.this.onRestoreInstanceState(mInstanceState);
                mInstanceState = null;
            } else {
                rememberSyncState();
            }
            checkFocus();
            requestLayout();
        }

        @Override
        public void onInvalidated() {
            mDataChanged = true;

            if (InnerAdapterView.this.getAdapter().hasStableIds()) {
                mInstanceState = InnerAdapterView.this.onSaveInstanceState();
            }
            mOldItemCount = mItemCount;
            mItemCount = 0;
            mNeedSync = false;

            checkFocus();
            requestLayout();
        }

        public void clearSavedState() {
            mInstanceState = null;
        }
    }
}
