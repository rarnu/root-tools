package com.rarnu.tools.neo.base;

import android.content.Context;

import java.util.List;

public abstract class BaseAdapter<T> extends InnerAdapter<T> {

    public BaseAdapter(Context context, List<T> list) {
        super(context, list);
    }

}
