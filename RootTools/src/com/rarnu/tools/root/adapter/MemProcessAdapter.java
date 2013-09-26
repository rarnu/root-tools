package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.holder.MemProcessAdapterHolder;

import java.util.List;

public class MemProcessAdapter extends BaseAdapter<MemProcessInfo> {

    public MemProcessAdapter(Context context, List<MemProcessInfo> list) {
        super(context, list);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {

        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.mem_process_item, parent, false);
        }
        MemProcessAdapterHolder holder = (MemProcessAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new MemProcessAdapterHolder();
            holder.item_icon = (ImageView) v.findViewById(R.id.item_icon);
            holder.item_name = (TextView) v.findViewById(R.id.item_name);
            holder.item_memory_used = (TextView) v.findViewById(R.id.item_memory_used);
            holder.item_namespace = (TextView) v.findViewById(R.id.item_namespace);
            v.setTag(holder);
        }
        MemProcessInfo item = list.get(position);
        if (item != null) {
            if (item.appInfo == null) {
                holder.item_icon.setBackgroundDrawable(v.getResources().getDrawable(R.drawable.android));
                holder.item_name.setText(item.NAME);
                holder.item_namespace.setText("");
            } else {
                holder.item_icon.setBackgroundDrawable(GlobalInstance.pm.getApplicationIcon(item.appInfo));
                holder.item_name.setText(GlobalInstance.pm.getApplicationLabel(item.appInfo));
                holder.item_namespace.setText(item.NAME);
            }
            holder.item_memory_used.setText(String.format("%dM", item.RSS));
        }
        return v;
    }

    @Override
    public String getValueText(MemProcessInfo item) {
        String valueText = item.NAME;
        if (item.appInfo != null) {
            valueText += GlobalInstance.pm.getApplicationLabel(item.appInfo);
        }
        return valueText;
    }
}
