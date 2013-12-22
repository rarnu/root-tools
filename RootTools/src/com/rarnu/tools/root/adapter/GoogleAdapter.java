package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.GoogleInfo;
import com.rarnu.tools.root.holder.GoogleAdapterHolder;

import java.util.List;

public class GoogleAdapter extends BaseAdapter<GoogleInfo> {

    public GoogleAdapter(Context context, List<GoogleInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(GoogleInfo item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.google_item, parent, false);
        }
        GoogleAdapterHolder holder = (GoogleAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new GoogleAdapterHolder();
            holder.name = (TextView) v.findViewById(R.id.item_name);
            holder.tvStatus = (TextView) v.findViewById(R.id.tvStatus);
            v.setTag(holder);
        }
        GoogleInfo item = list.get(position);
        if (item != null) {
            holder.name.setText(item.fileName);
            switch (item.status) {
                case -1:
                    holder.tvStatus.setText(R.string.google_status_tobechecked);
                    holder.tvStatus.setTextColor(Color.YELLOW);
                    break;
                case 0:
                    holder.tvStatus.setText(R.string.google_status_normal);
                    holder.tvStatus.setTextColor(Color.GREEN);
                    break;
                case 1:
                    holder.tvStatus.setText(R.string.google_status_not_found);
                    holder.tvStatus.setTextColor(Color.RED);
                    break;
                case 2:
                    holder.tvStatus.setText(R.string.google_status_file_boken);
                    holder.tvStatus.setTextColor(Color.RED);
                    break;
            }

        }
        return v;
    }
}
