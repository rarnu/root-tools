package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.AboutInfo;
import com.rarnu.tools.root.holder.AboutAdapterHolder;

import java.util.List;

public class AboutAdapter extends BaseAdapter<AboutInfo> {

    int[] fitableDrawable = new int[]{
            R.drawable.c0, R.drawable.c1, R.drawable.c2, R.drawable.c3, R.drawable.c4,
            R.drawable.c5, R.drawable.c6, R.drawable.c7, R.drawable.c8, R.drawable.c9};

    public AboutAdapter(Context context, List<AboutInfo> list) {
        super(context, list);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.about_item, parent, false);
        }
        AboutAdapterHolder holder = (AboutAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new AboutAdapterHolder();
            holder.tvTitle = (TextView) v.findViewById(R.id.tvTitle);
            holder.imgFitable = (ImageView) v.findViewById(R.id.imgFitable);
            v.setTag(holder);
        }
        AboutInfo item = list.get(position);
        if (item != null) {
            holder.tvTitle.setText(item.title);
            holder.imgFitable.setVisibility(item.fitable == -1 ? View.GONE : View.VISIBLE);
            if (item.fitable != -1) {
                holder.imgFitable.setImageResource(fitableDrawable[item.fitable]);
            }
        }
        return v;
    }

    @Override
    public String getValueText(AboutInfo item) {
        return "";
    }

}
