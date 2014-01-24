package com.rarnu.fontter.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;
import com.rarnu.fontter.R;

import java.util.List;

public class FontAdapter extends BaseAdapter {

    private List<FontItem> list;
    private Context context;
    private LayoutInflater inflater;

    public FontAdapter(Context context, List<FontItem> list) {
        this.context = context;
        this.list = list;
        this.inflater = LayoutInflater.from(context);
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
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.font_item, parent, false);
        }
        FontHolder holder = (FontHolder) v.getTag();
        if (holder == null) {
            holder = new FontHolder();
            holder.tvName = (TextView) v.findViewById(R.id.tvName);
            holder.tvStatus = (TextView) v.findViewById(R.id.tvState);
            v.setTag(holder);
        }
        FontItem item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.name);
            holder.tvStatus.setText(item.inUse ? context.getString(R.string.font_item_using) : "");
        }
        return v;
    }
}
