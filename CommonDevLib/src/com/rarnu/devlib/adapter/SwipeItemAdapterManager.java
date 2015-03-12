package com.rarnu.devlib.adapter;

import android.view.View;
import android.widget.BaseAdapter;
import com.rarnu.devlib.component.SwipeLayout;

public class SwipeItemAdapterManager extends SwipeItemManager {

    protected BaseAdapter mAdapter;

    public SwipeItemAdapterManager(BaseAdapter adapter) {
        super(adapter);
        this.mAdapter = adapter;
    }

    @Override
    public void initialize(View target, int position) {
        int resId = getSwipeLayoutId(position);

        OnLayoutListener onLayoutListener = new OnLayoutListener(position);
        SwipeLayout swipeLayout = (SwipeLayout) target.findViewById(resId);
        if (swipeLayout == null)
            throw new IllegalStateException("can not find SwipeLayout in target view");

        SwipeMemory swipeMemory = new SwipeMemory(position);
        swipeLayout.addSwipeListener(swipeMemory);
        swipeLayout.addOnLayoutListener(onLayoutListener);
        swipeLayout.setTag(resId, new ValueBox(position, swipeMemory, onLayoutListener));

        mShownLayouts.add(swipeLayout);
    }

    @Override
    public void updateConvertView(View target, int position) {
        int resId = getSwipeLayoutId(position);

        SwipeLayout swipeLayout = (SwipeLayout) target.findViewById(resId);
        if (swipeLayout == null)
            throw new IllegalStateException("can not find SwipeLayout in target view");

        ValueBox valueBox = (ValueBox) swipeLayout.getTag(resId);
        valueBox.swipeMemory.setPosition(position);
        valueBox.onLayoutListener.setPosition(position);
        valueBox.position = position;
    }

    @Override
    public void bindView(View target, int position){

    }

}
