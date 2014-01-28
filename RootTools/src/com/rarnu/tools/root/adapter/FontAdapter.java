package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Color;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FontItem;
import com.rarnu.tools.root.holder.FontHolder;

import java.util.List;

public class FontAdapter extends BaseAdapter<FontItem> {

    public FontAdapter(Context context, List<FontItem> list) {
        super(context, list);
    }

    @Override
    public String getValueText(FontItem item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.fonts_item, parent, false);
        }
        FontHolder holder = (FontHolder) v.getTag();
        if (holder == null) {
            holder = new FontHolder();
            holder.tvName = (TextView) v.findViewById(R.id.tvName);
            holder.tvStatus = (TextView) v.findViewById(R.id.tvState);
            v.setTag(holder);
        }
        final FontItem item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.name);
            holder.tvName.setTextColor(item.inUse ? context.getResources().getColor(R.color.greenyellow) : Color.WHITE);
            holder.tvStatus.setText(item.isDownloaded ? context.getString(R.string.font_downloaded) : "");
        }
        return v;
    }
}
