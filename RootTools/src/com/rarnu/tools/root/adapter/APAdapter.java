package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.net.wifi.ScanResult;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.holder.APAdapterHolder;

import java.util.List;

public class APAdapter extends BaseAdapter<ScanResult> {

    public APAdapter(Context context, List<ScanResult> list) {
        super(context, list);
    }

    @Override
    public String getValueText(ScanResult item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.ap_item, parent, false);
        }
        APAdapterHolder holder = (APAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new APAdapterHolder();
            holder.tvApName = (TextView) v.findViewById(R.id.tvApName);
            v.setTag(holder);
        }
        ScanResult item = list.get(position);
        if (item != null) {
            holder.tvApName.setText(item.SSID);
        }
        return v;
    }
}
