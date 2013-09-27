package com.yugioh.android.adapter;


import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
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
            v.setTag(holder);
        }
        PackageItem item = list.get(position);
        if (item != null) {
            holder.tvPackName.setText(item.name);
            v.setLayoutParams(item.isPackageTitle ? allpTitle : allpList);
            v.setBackgroundColor(context.getResources().getColor(item.isPackageTitle ? android.R.color.holo_blue_light : R.color.transparent));
        }
        return v;
    }
}
