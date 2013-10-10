package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.SysappSelectApkItem;
import com.rarnu.tools.root.holder.SysappSelectApkAdapterHolder;
import com.rarnu.utils.DrawableUtils;

import java.util.List;

public class SysappSelectApkAdapter extends BaseAdapter<SysappSelectApkItem> {

    public SysappSelectApkAdapter(Context context, List<SysappSelectApkItem> list) {
        super(context, list);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {

        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.sysapp_file_item, parent, false);
        }

        SysappSelectApkAdapterHolder holder = (SysappSelectApkAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new SysappSelectApkAdapterHolder();
            holder.imgIcon = (ImageView) v.findViewById(R.id.img_icon);
            holder.tvFilename = (TextView) v.findViewById(R.id.tv_filename);
            v.setTag(holder);
        }
        SysappSelectApkItem item = list.get(position);
        if (item != null) {
            if (item.icon == 1) {
                holder.imgIcon.setBackgroundDrawable(v.getResources().getDrawable(R.drawable.format_folder));
            } else {
                holder.imgIcon.setBackgroundDrawable(item.iconImg);
            }
            holder.tvFilename.setText(item.filename);

            holder.tvFilename.setTextColor(DrawableUtils.getTextColorPrimary(context));

            switch (item.level) {
                case 0:
                    holder.tvFilename.setTextColor(Color.RED);
                    break;
                case 1:
                    holder.tvFilename.setTextColor(Color.GREEN);
                    break;
                case 2:
                    holder.tvFilename.setTextColor(0xFF6495ED);
                    break;
            }

        }

        return v;
    }

    @Override
    public String getValueText(SysappSelectApkItem item) {
        return item.filename;
    }

}
