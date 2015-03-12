package com.rarnu.devlib.adapter;

import android.content.Context;
import android.database.Cursor;
import android.support.v4.widget.CursorAdapter;
import android.view.View;
import android.view.ViewGroup;

import com.rarnu.devlib.component.SwipeLayout;
import com.rarnu.devlib.intf.SwipeAdapterInterface;
import com.rarnu.devlib.intf.SwipeItemManageIntf;

import java.util.List;

public abstract class CursorSwipeAdapter extends CursorAdapter implements SwipeItemManageIntf, SwipeAdapterInterface {

    private SwipeItemAdapterManager mItemManger = new SwipeItemAdapterManager(this);

    protected CursorSwipeAdapter(Context context, Cursor c, boolean autoRequery) {
        super(context, c, autoRequery);
    }

    protected CursorSwipeAdapter(Context context, Cursor c, int flags) {
        super(context, c, flags);
    }

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
