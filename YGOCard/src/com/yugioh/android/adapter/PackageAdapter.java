package com.yugioh.android.adapter;


import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.utils.DrawableUtils;
import com.rarnu.utils.UIUtils;
import com.yugioh.android.R;
import com.yugioh.android.classes.PackageItem;

import java.util.List;

public class PackageAdapter extends BaseAdapter<PackageItem> {

    private AbsListView.LayoutParams allpTitle;
    private AbsListView.LayoutParams allpList;

    public PackageAdapter(Context context, List<PackageItem> list) {
        super(context, list);
        allpTitle = new AbsListView.LayoutParams(AbsListView.LayoutParams.MATCH_PARENT, UIUtils.dipToPx(24));
        allpList = new AbsListView.LayoutParams(AbsListView.LayoutParams.MATCH_PARENT, UIUtils.dipToPx(48));
    }

    @Override
    public String getValueText(PackageItem item) {
        return "";
    }

    @Override
    public boolean isEnabled(int position) {
        return !list.get(position).isPackageTitle;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item_package, parent, false);
        }
        PackageHolder holder = (PackageHolder) v.getTag();
        if (holder == null) {
            holder = new PackageHolder();
            holder.tvPackName = (TextView) v.findViewById(R.id.tvPackName);
            holder.tvLine = (TextView) v.findViewById(R.id.tvLine);
            v.setTag(holder);
        }
        PackageItem item = list.get(position);
        if (item != null) {
            holder.tvPackName.setText(item.name);
            if (item.isPackageTitle) {
                holder.tvPackName.setTextColor(context.getResources().getColor(R.color.orange));
            } else {
                holder.tvPackName.setTextColor(DrawableUtils.getTextColorPrimary(context));
            }
            holder.tvPackName.getPaint().setFakeBoldText(item.isPackageTitle);
            holder.tvLine.setVisibility(item.isPackageTitle ? View.VISIBLE : View.GONE);
            v.setLayoutParams(item.isPackageTitle ? allpTitle : allpList);

        }
        return v;
    }
}
