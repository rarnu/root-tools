package com.rarnu.tools.neo.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.Switch;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseAdapter;
import com.rarnu.tools.neo.data.AppInfo;
import com.rarnu.tools.neo.data.BanStartInfo;

import java.util.List;

public class BannedAdapter extends BaseAdapter<BanStartInfo> {

    public BannedAdapter(Context context, List<BanStartInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(BanStartInfo item) {
        return item.name + item.packageName;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.listitem_app, parent, false);
        }
        BannedHolder holder = (BannedHolder) v.getTag();
        if (holder == null) {
            holder = new BannedHolder(v, R.id.ivIcon, R.id.prefStatus, R.id.tvName, R.id.tvPackageName);
            v.setTag(holder);
        }
        BanStartInfo item = list.get(position);
        holder.setItem(item);
        holder.prefStatus.setVisibility(View.VISIBLE);
        return v;
    }

    private class BannedHolder {
        ImageView ivIcon = null;
        Switch prefStatus = null;
        TextView tvName = null;
        TextView tvPackageName = null;

        BannedHolder(View v, int iconId, int statusId, int nameId, int pkgId) {
            ivIcon = (ImageView) v.findViewById(iconId);
            prefStatus = (Switch) v.findViewById(statusId);
            tvName = (TextView) v.findViewById(nameId);
            tvPackageName = (TextView) v.findViewById(pkgId);
        }

        void setItem(BanStartInfo item) {
            ivIcon.setImageDrawable(item.imageId);
            prefStatus.setChecked(item.isBanned);
            tvName.setText(item.name);
            tvPackageName.setText(item.packageName);
            tvName.setTextColor(context.getColor(item.isSystem ? android.R.color.holo_green_dark : android.R.color.primary_text_light));
            tvPackageName.setTextColor(context.getColor(item.isSystem ? android.R.color.holo_green_light : android.R.color.secondary_text_light));
        }
    }

}
