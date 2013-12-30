package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.RemainedInfo;
import com.rarnu.tools.root.holder.RemainedAdapterHolder;

import java.util.List;

public class RemainedAdapter extends BaseAdapter<RemainedInfo> {
    public RemainedAdapter(Context context, List<RemainedInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(RemainedInfo item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.remained_item, parent, false);
        }
        RemainedAdapterHolder holder = (RemainedAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new RemainedAdapterHolder();
            holder.tvName = (TextView) v.findViewById(R.id.item_name);
            holder.tvPath = (TextView) v.findViewById(R.id.item_path);
            holder.tvSize = (TextView) v.findViewById(R.id.tvSize);
            v.setTag(holder);
        }

        RemainedInfo item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.packageName);
            holder.tvPath.setText(item.path);
            holder.tvSize.setText(item.dirSize);
            holder.tvSize.setTextColor(Color.GREEN);
        }
        return v;
    }
}
