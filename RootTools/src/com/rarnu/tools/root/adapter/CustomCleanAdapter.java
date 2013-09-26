package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.os.Handler;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.CustomPackageInfo;
import com.rarnu.tools.root.holder.CustomCleanAdapterHolder;

import java.util.List;

public class CustomCleanAdapter extends BaseAdapter<CustomPackageInfo> {

    private Handler h;

    public CustomCleanAdapter(Context context, List<CustomPackageInfo> list, Handler h) {
        super(context, list);
        this.h = h;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.custom_clean_item, parent, false);
        }
        CustomCleanAdapterHolder holder = (CustomCleanAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new CustomCleanAdapterHolder();
            holder.itemTitle = (TextView) v.findViewById(R.id.itemTitle);
            holder.itemNamespace = (TextView) v.findViewById(R.id.itemNamespace);
            holder.chkSelect = (CheckBox) v.findViewById(R.id.chkSelect);
            v.setTag(holder);
        }
        final CustomPackageInfo item = list.get(position);
        if (item != null) {
            holder.itemTitle.setText(item.title);
            holder.itemNamespace.setText(item.namespace);
            holder.chkSelect.setChecked(item.checked);
            holder.chkSelect.setOnClickListener(new OnClickListener() {

                @Override
                public void onClick(View v) {
                    item.checked = ((CheckBox) v).isChecked();
                    h.sendEmptyMessage(1);
                }
            });
        }
        return v;
    }

    @Override
    public String getValueText(CustomPackageInfo item) {
        return "";
    }

}
