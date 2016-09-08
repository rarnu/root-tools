package com.rarnu.tools.neo.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.Switch;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseAdapter;
import com.rarnu.tools.neo.data.AppInfo;

import java.util.List;

public class AppAdapter extends BaseAdapter<AppInfo> {

    private boolean showSwitch = false;

    public AppAdapter(Context context, List<AppInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(AppInfo item) {
        return item.name + item.packageName;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.listitem_app, parent, false);
        }
        AppHolder holder = (AppHolder) v.getTag();
        if (holder == null) {
            holder = new AppHolder(v, R.id.ivIcon, R.id.prefStatus, R.id.tvName, R.id.tvPackageName);
            v.setTag(holder);
        }
        AppInfo item = list.get(position);
        holder.setItem(item);
        holder.prefStatus.setVisibility(showSwitch ? View.VISIBLE : View.GONE);
        return v;
    }

    public void setShowSwitch(boolean on) {
        showSwitch = on;
        notifyDataSetChanged();
    }

    private class AppHolder {
        ImageView ivIcon = null;
        Switch prefStatus = null;
        TextView tvName = null;
        TextView tvPackageName = null;

        AppHolder(View v, int iconId, int statusId, int nameId, int pkgId) {
            ivIcon = (ImageView) v.findViewById(iconId);
            prefStatus = (Switch) v.findViewById(statusId);
            tvName = (TextView) v.findViewById(nameId);
            tvPackageName = (TextView) v.findViewById(pkgId);
        }

        void setItem(AppInfo item) {
            ivIcon.setImageDrawable(item.imageId);
            prefStatus.setChecked(!item.isDisable);
            tvName.setText(item.name);
            tvPackageName.setText(item.packageName);
            if (item.isForFreeze) {
                tvName.setTextColor(Color.BLACK);
                tvPackageName.setTextColor(Color.DKGRAY);
            } else {
                if (item.isSystem) {
                    tvName.setTextColor(context.getResources().getColor(android.R.color.holo_green_dark));
                    tvPackageName.setTextColor(context.getResources().getColor(android.R.color.holo_green_light));
                } else {
                    tvName.setTextColor(Color.BLACK);
                    tvPackageName.setTextColor(Color.DKGRAY);
                }
            }
        }
    }

}
