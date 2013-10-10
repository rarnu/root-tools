package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FileOperationInfo;
import com.rarnu.tools.root.holder.PoolHolder;

import java.util.List;

public class PoolAdapter extends BaseAdapter<FileOperationInfo> {
    public PoolAdapter(Context context, List<FileOperationInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(FileOperationInfo item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.pool_item, parent, false);
        }
        PoolHolder holder = (PoolHolder) v.getTag();
        if (holder == null) {
            holder = new PoolHolder();
            holder.imgIcon = (ImageView) v.findViewById(R.id.imgIcon);
            holder.tvName = (TextView) v.findViewById(R.id.tvName);
            holder.imgOperation = (ImageView) v.findViewById(R.id.imgOperation);
            v.setTag(holder);
        }
        FileOperationInfo item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.name);
            holder.imgOperation.setImageResource(item.operation == 0 ? R.drawable.cut : R.drawable.copy);
            holder.imgIcon.setBackgroundResource(item.icon);
        }
        return v;
    }
}
