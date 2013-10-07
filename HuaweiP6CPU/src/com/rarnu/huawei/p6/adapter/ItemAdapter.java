package com.rarnu.huawei.p6.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;
import com.rarnu.huawei.p6.R;

public class ItemAdapter extends BaseAdapter {

    private Context context;
    private LayoutInflater inflater;
    private String[] items;

    public ItemAdapter(Context context, String[] items) {
        this.context = context;
        inflater = LayoutInflater.from(context);
        this.items = items;
    }

    @Override
    public int getCount() {
        return items.length;
    }

    @Override
    public Object getItem(int position) {
        return items[position];
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item, parent, false);
        }
        ItemHolder holder = (ItemHolder) v.getTag();
        if (holder == null) {
            holder = new ItemHolder();
            holder.tvItem = (TextView) v.findViewById(R.id.tvItem);
            v.setTag(holder);
        }
        String item = items[position];
        if (item != null) {
            holder.tvItem.setText(item.equals("0") ? context.getString(R.string.str_default) : item);
        }
        return v;
    }
}
