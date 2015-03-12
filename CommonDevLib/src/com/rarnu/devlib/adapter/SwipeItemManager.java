package com.rarnu.devlib.adapter;

import android.view.View;
import android.widget.BaseAdapter;

import com.rarnu.devlib.component.SwipeLayout;
import com.rarnu.devlib.intf.SimpleSwipeListener;
import com.rarnu.devlib.intf.SwipeAdapterInterface;
import com.rarnu.devlib.intf.SwipeItemManageIntf;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class SwipeItemManager implements SwipeItemManageIntf {

    private SwipeLayout.Mode mode = SwipeLayout.Mode.Single;
    public final int INVALID_POSITION = -1;

    protected int mOpenPosition = INVALID_POSITION;

    protected Set<Integer> mOpenPositions = new HashSet<Integer>();
    protected Set<SwipeLayout> mShownLayouts = new HashSet<SwipeLayout>();

    protected BaseAdapter mBaseAdapter;

    public SwipeItemManager(BaseAdapter adapter) {
        if (adapter == null)
            throw new IllegalArgumentException("Adapter can not be null");

        if (!(adapter instanceof SwipeItemManageIntf))
            throw new IllegalArgumentException("adapter should implement the SwipeAdapterInterface");

        this.mBaseAdapter = adapter;
    }

    public SwipeLayout.Mode getMode() {
        return mode;
    }

    public void setMode(SwipeLayout.Mode mode) {
        this.mode = mode;
        mOpenPositions.clear();
        mShownLayouts.clear();
        mOpenPosition = INVALID_POSITION;
    }

    public abstract void initialize(View target, int position);

    public abstract void updateConvertView(View target, int position);

    public abstract void bindView(View target, int position);

    public int getSwipeLayoutId(int position) {
        if (mBaseAdapter != null) {
            return ((SwipeAdapterInterface) (mBaseAdapter)).getSwipeLayoutResourceId(position);
        } else {
            return -1;
        }
    }

    @Override
    public void openItem(int position) {
        if (mode == SwipeLayout.Mode.Multiple) {
            if (!mOpenPositions.contains(position))
                mOpenPositions.add(position);
        } else {
            mOpenPosition = position;
        }
        if (mBaseAdapter != null) {
            mBaseAdapter.notifyDataSetChanged();
        }
    }

    @Override
    public void closeItem(int position) {
        if (mode == SwipeLayout.Mode.Multiple) {
            mOpenPositions.remove(position);
        } else {
            if (mOpenPosition == position)
                mOpenPosition = INVALID_POSITION;
        }
        if (mBaseAdapter != null) {
            mBaseAdapter.notifyDataSetChanged();
        }
    }

    @Override
    public void closeAllExcept(SwipeLayout layout) {
        for (SwipeLayout s : mShownLayouts) {
            if (s != layout)
                s.close();
        }
    }

    @Override
    public void closeAllItems() {
        if (mode == SwipeLayout.Mode.Multiple) {
            mOpenPositions.clear();
        } else {
            mOpenPosition = INVALID_POSITION;
        }
        for (SwipeLayout s : mShownLayouts) {
            s.close();
        }
    }

    @Override
    public void removeShownLayouts(SwipeLayout layout) {
        mShownLayouts.remove(layout);
    }

    @Override
    public List<Integer> getOpenItems() {
        if (mode == SwipeLayout.Mode.Multiple) {
            return new ArrayList<Integer>(mOpenPositions);
        } else {
            return Arrays.asList(mOpenPosition);
        }
    }

    @Override
    public List<SwipeLayout> getOpenLayouts() {
        return new ArrayList<SwipeLayout>(mShownLayouts);
    }

    @Override
    public boolean isOpen(int position) {
        if (mode == SwipeLayout.Mode.Multiple) {
            return mOpenPositions.contains(position);
        } else {
            return mOpenPosition == position;
        }
    }

    class ValueBox {
        OnLayoutListener onLayoutListener;
        SwipeMemory swipeMemory;
        int position;

        ValueBox(int position, SwipeMemory swipeMemory, OnLayoutListener onLayoutListener) {
            this.swipeMemory = swipeMemory;
            this.onLayoutListener = onLayoutListener;
            this.position = position;
        }
    }

    class OnLayoutListener implements SwipeLayout.OnLayout {

        private int position;

        OnLayoutListener(int position) {
            this.position = position;
        }

        public void setPosition(int position) {
            this.position = position;
        }

        @Override
        public void onLayout(SwipeLayout v) {
            if (isOpen(position)) {
                v.open(false, false);
            } else {
                v.close(false, false);
            }
        }

    }

    class SwipeMemory extends SimpleSwipeListener {

        private int position;

        SwipeMemory(int position) {
            this.position = position;
        }

        @Override
        public void onClose(SwipeLayout layout) {
            if (mode == SwipeLayout.Mode.Multiple) {
                mOpenPositions.remove(position);
            } else {
                mOpenPosition = INVALID_POSITION;
            }
        }

        @Override
        public void onStartOpen(SwipeLayout layout) {
            if (mode == SwipeLayout.Mode.Single) {
                closeAllExcept(layout);
            }
        }

        @Override
        public void onOpen(SwipeLayout layout) {
            if (mode == SwipeLayout.Mode.Multiple)
                mOpenPositions.add(position);
            else {
                closeAllExcept(layout);
                mOpenPosition = position;
            }
        }

        public void setPosition(int position) {
            this.position = position;
        }
    }

}
