package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.IptablePackageInfo;
import com.rarnu.tools.root.holder.IptablesHolder;

import java.util.List;

public class IptablesAdapter extends BaseAdapter<IptablePackageInfo> {

    public IptablesAdapter(Context context, List<IptablePackageInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(IptablePackageInfo item) {
        return item.toString().toLowerCase() + (item.appinfo != null ? item.appinfo.packageName.toLowerCase() : "");
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {

        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.firewall_item, parent, false);
        }
        IptablesHolder holder = (IptablesHolder) v.getTag();
        if (holder == null) {

            holder = new IptablesHolder();
            holder.chkWifi = (CheckBox) v.findViewById(R.id.chk_wifi);
            holder.chk3g = (CheckBox) v.findViewById(R.id.chk_3g);
            holder.name = (TextView) v.findViewById(R.id.item_name);
            holder.icon = (ImageView) v.findViewById(R.id.item_icon);

            v.setTag(holder);
        }
        final IptablePackageInfo item = list.get(position);
        if (item != null) {
            holder.app = item;
            holder.name.setText(item.toString());
            holder.icon.setBackgroundDrawable(item.appinfo != null ? pm.getApplicationIcon(item.appinfo) : null);
            holder.chkWifi.setChecked(item.selected_wifi);
            holder.chk3g.setChecked(item.selected_3g);
            holder.chkWifi.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    item.selected_wifi = ((CheckBox) v).isChecked();
                }
            });
            holder.chk3g.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    item.selected_3g = ((CheckBox) v).isChecked();
                }
            });

        }
        return v;
    }
}
