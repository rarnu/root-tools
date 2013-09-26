package com.rarnu.devlib.base.adapter;

import android.content.Context;
import com.rarnu.devlib.base.inner.InnerAdapter;

import java.util.List;

public abstract class BaseAdapter<T> extends InnerAdapter<T> {

    public BaseAdapter(Context context, List<T> list) {
        super(context, list);
    }

}
