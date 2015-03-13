package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.AutobootInfo;
import com.rarnu.tools.root.holder.AutobootAdapterHolder;

import java.util.List;

public class AutobootAdapter extends BaseAdapter<AutobootInfo> {

    public AutobootAdapter(Context context, List<AutobootInfo> list) {
        super(context, list);
    }

    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {

        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.autoboot_item, parent, false);
        }
        AutobootAdapterHolder holder = (AutobootAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new AutobootAdapterHolder();
            holder.icon = (ImageView) v.findViewById(R.id.item_icon);
            holder.name = (TextView) v.findViewById(R.id.item_name);
            holder.packageName = (TextView) v.findViewById(R.id.item_package);
            holder.tvEnabled = (TextView) v.findViewById(R.id.tvEnabled);

            v.setTag(holder);
        }
        final AutobootInfo item = list.get(position);
        if (item != null) {
            holder.icon.setBackgroundDrawable(pm.getApplicationIcon(item.info.applicationInfo));
            holder.name.setText(pm.getApplicationLabel(item.info.applicationInfo));
            holder.packageName.setText(item.info.packageName);
            holder.tvEnabled.setText(item.enabled ? R.string.package_enabled : R.string.package_disabled);
            holder.tvEnabled.setTextColor(item.enabled ? Color.GREEN : Color.RED);

            if (item.processing) {
                holder.tvEnabled.setTextColor(Color.YELLOW);
                holder.tvEnabled.setText(R.string.loading);
            }
        }

        return v;
    }

    @Override
    public String getValueText(AutobootInfo item) {
        return pm.getApplicationLabel(item.info.applicationInfo).toString() + item.info.packageName;
    }
}
