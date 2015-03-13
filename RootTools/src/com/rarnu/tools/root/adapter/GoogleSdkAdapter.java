package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.GooglePackageInfo;
import com.rarnu.tools.root.holder.GoogleSdkAdapterHolder;

import java.util.List;

public class GoogleSdkAdapter extends BaseAdapter<GooglePackageInfo> {

    public GoogleSdkAdapter(Context context, List<GooglePackageInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(GooglePackageInfo item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.google_sdk_item, parent, false);
        }
        GoogleSdkAdapterHolder holder = (GoogleSdkAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new GoogleSdkAdapterHolder();
            holder.tvName = (TextView) v.findViewById(R.id.tvName);
            v.setTag(holder);
        }
        GooglePackageInfo item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.sdk_name);
        }
        return v;
    }
}
