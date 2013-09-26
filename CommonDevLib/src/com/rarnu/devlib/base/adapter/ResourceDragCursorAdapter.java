package com.rarnu.devlib.base.adapter;

import android.content.Context;
import android.database.Cursor;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public abstract class ResourceDragCursorAdapter extends BaseDragCursorAdapter {
    private int mLayout;
    private int mDropDownLayout;
    private LayoutInflater mInflater;

    public ResourceDragCursorAdapter(Context context, int layout, Cursor c, boolean autoRequery) {
        super(context, c, autoRequery);
        init(context, layout);
    }

    public ResourceDragCursorAdapter(Context context, int layout, Cursor c, int flags) {
        super(context, c, flags);
        init(context, layout);
    }

    private void init(Context context, int layout) {
        mLayout = mDropDownLayout = layout;
        mInflater = LayoutInflater.from(context);
    }

    @Override
    public View newView(Context context, Cursor cursor, ViewGroup parent) {
        return mInflater.inflate(mLayout, parent, false);
    }

    @Override
    public View newDropDownView(Context context, Cursor cursor, ViewGroup parent) {
        return mInflater.inflate(mDropDownLayout, parent, false);
    }

    public void setViewResource(int layout) {
        mLayout = layout;
    }

    public void setDropDownViewResource(int dropDownLayout) {
        mDropDownLayout = dropDownLayout;
    }
}
