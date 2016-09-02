package com.rarnu.tools.neo.base;

import android.content.Context;
import android.content.pm.PackageManager;
import android.view.LayoutInflater;
import android.widget.BaseAdapter;
import android.widget.Filter;
import android.widget.Filterable;

import java.util.ArrayList;
import java.util.List;

public abstract class InnerAdapter<T> extends BaseAdapter implements Filterable {

    protected final Object lock = new Object();
    protected Context context;
    protected LayoutInflater inflater;
    protected List<T> listFull;
    protected List<T> list;
    protected ArrayFilter filter;
    protected PackageManager pm;

    public InnerAdapter(Context context, List<T> list) {
        this.context = context;
        this.inflater = LayoutInflater.from(context);
        this.listFull = list;
        this.list = list;
        this.pm = context.getPackageManager();
    }

    public void setNewList(List<T> list) {
        this.listFull = list;
        this.list = list;
        this.notifyDataSetChanged();
    }

    public void deleteItem(T item) {
        this.list.remove(item);
        this.listFull.remove(item);
        notifyDataSetChanged();
    }

    public void deleteItems(List<T> items) {
        for (T info : items) {
            list.remove(info);
            listFull.remove(info);
        }
        notifyDataSetChanged();
    }

    @Override
    public int getCount() {
        return list.size();
    }

    @Override
    public Object getItem(int position) {
        return list.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public Filter getFilter() {
        if (filter == null) {
            filter = new ArrayFilter();
        }
        return filter;
    }

    public abstract String getValueText(T item);

    public void filter(String text) {
        getFilter().filter(text);
    }

    public T getFiltedItem(int position) {
        return list.get(position);
    }

    public class ArrayFilter extends Filter {

        @Override
        protected FilterResults performFiltering(CharSequence prefix) {
            list = listFull;
            FilterResults results = new FilterResults();
            if (prefix == null || prefix.length() == 0) {
                synchronized (lock) {
                    ArrayList<T> l = new ArrayList<T>(list);
                    results.values = l;
                    results.count = l.size();
                }
            } else {
                String prefixString = prefix.toString().toLowerCase();
                final ArrayList<T> values = new ArrayList<T>(list);
                final int count = values.size();
                final ArrayList<T> newValues = new ArrayList<T>(count);
                for (int i = 0; i < count; i++) {
                    final T value = values.get(i);
                    final String valueText = getValueText(value).toLowerCase();
                    if (valueText.contains(prefixString)) {
                        newValues.add(value);
                    }
                }
                results.values = newValues;
                results.count = newValues.size();
            }
            return results;
        }

        @SuppressWarnings("unchecked")
        @Override
        protected void publishResults(CharSequence constraint, FilterResults results) {
            list = (List<T>) results.values;
            if (results.count > 0) {
                notifyDataSetChanged();
            } else {
                notifyDataSetInvalidated();
            }
        }

    }

}
