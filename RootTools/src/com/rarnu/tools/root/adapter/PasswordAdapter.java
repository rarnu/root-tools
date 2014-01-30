package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.PasswordItem;
import com.rarnu.tools.root.holder.PasswordAdapterHolder;

import java.util.List;

public class PasswordAdapter extends BaseAdapter<PasswordItem> {

    public PasswordAdapter(Context context, List<PasswordItem> list) {
        super(context, list);
    }

    @Override
    public String getValueText(PasswordItem item) {
        return item.name.toLowerCase();
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.password_item, parent, false);
        }
        PasswordAdapterHolder holder = (PasswordAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new PasswordAdapterHolder();
            holder.tvName = (TextView) v.findViewById(R.id.item_name);
            v.setTag(holder);
        }

        PasswordItem item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.name);
        }
        return v;
    }
}
