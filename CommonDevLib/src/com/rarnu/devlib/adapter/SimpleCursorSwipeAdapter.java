package com.rarnu.devlib.adapter;

import android.content.Context;
import android.database.Cursor;
import android.support.v4.widget.SimpleCursorAdapter;
import android.view.View;
import android.view.ViewGroup;

import com.rarnu.devlib.component.SwipeLayout;
import com.rarnu.devlib.intf.SwipeAdapterInterface;
import com.rarnu.devlib.intf.SwipeItemManageIntf;

import java.util.List;

public abstract class SimpleCursorSwipeAdapter extends SimpleCursorAdapter implements SwipeItemManageIntf, SwipeAdapterInterface {

    private SwipeItemAdapterManager mItemManger = new SwipeItemAdapterManager(this);

    protected SimpleCursorSwipeAdapter(Context context, int layout, Cursor c, String[] from, int[] to, int flags) {
        super(context, layout, c, from, to, flags);
    }

//    protected SimpleCursorSwipeAdapter(Context context, int layout, Cursor c, String[] from, int[] to) {
//        super(context, layout, c, from, to);
//    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        boolean convertViewIsNull = convertView == null;
        View v = super.getView(position, convertView, parent);
        if(convertViewIsNull){
            mItemManger.initialize(v, position);
        }else{
            mItemManger.updateConvertView(v, position);
        }
        return v;
    }

    @Override
    public void openItem(int position) {
        mItemManger.openItem(position);
    }

    @Override
    public void closeItem(int position) {
        mItemManger.closeItem(position);
    }

    @Override
    public void closeAllExcept(SwipeLayout layout) {
        mItemManger.closeAllExcept(layout);
    }

    @Override
    public List<Integer> getOpenItems() {
        return mItemManger.getOpenItems();
    }

    @Override
    public List<SwipeLayout> getOpenLayouts() {
        return mItemManger.getOpenLayouts();
    }

    @Override
    public void removeShownLayouts(SwipeLayout layout) {
        mItemManger.removeShownLayouts(layout);
    }

    @Override
    public boolean isOpen(int position) {
        return mItemManger.isOpen(position);
    }

    @Override
    public SwipeLayout.Mode getMode() {
        return mItemManger.getMode();
    }

    @Override
    public void setMode(SwipeLayout.Mode mode) {
        mItemManger.setMode(mode);
    }
}
