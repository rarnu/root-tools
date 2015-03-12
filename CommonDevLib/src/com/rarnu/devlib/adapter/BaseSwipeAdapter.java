package com.rarnu.devlib.adapter;

import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;

import com.rarnu.devlib.component.SwipeLayout;
import com.rarnu.devlib.intf.SwipeAdapterInterface;
import com.rarnu.devlib.intf.SwipeItemManageIntf;

import java.util.List;

public abstract class BaseSwipeAdapter extends BaseAdapter implements SwipeItemManageIntf, SwipeAdapterInterface {

    protected SwipeItemAdapterManager mItemManger = new SwipeItemAdapterManager(this);

    public abstract int getSwipeLayoutResourceId(int position);

    public abstract View generateView(int position, ViewGroup parent);

    public abstract void fillValues(int position, View convertView);

    @Override
    public final View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if(v == null){
            v = generateView(position, parent);
            mItemManger.initialize(v, position);
        }else{
            mItemManger.updateConvertView(v, position);
        }
        fillValues(position, v);
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
    public void closeAllItems() {
        mItemManger.closeAllItems();
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
