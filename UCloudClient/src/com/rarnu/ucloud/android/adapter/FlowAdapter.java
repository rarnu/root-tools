package com.rarnu.ucloud.android.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.pojo.FlowItem;

import java.util.List;

public class FlowAdapter extends BaseAdapter<FlowItem> {
    public FlowAdapter(Context context, List<FlowItem> list) {
        super(context, list);
    }

    @Override
    public String getValueText(FlowItem item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item_flow, parent, false);
        }
        FlowHolder holder = (FlowHolder) v.getTag();
        if (holder == null) {
            holder = new FlowHolder();
            v.setTag(holder);
        }

        FlowItem item = list.get(position);
        if (item != null) {
            // TODO:
        }

        return v;
    }
}
