package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.FontAPI;
import com.rarnu.tools.root.common.FontItem;
import com.rarnu.tools.root.holder.FontHolder;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.DownloadUtils;

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

            holder.ivPreview = (ImageView) v.findViewById(R.id.ivPreview);
            v.setTag(holder);
        }
        final FontItem item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.name);
            holder.tvName.setTextColor(item.inUse ? context.getResources().getColor(R.color.greenyellow) : Color.WHITE);

            if (item.preview != null && !item.preview.equals("") && !item.preview.equals("null")) {
                DownloadUtils.downloadFileT(context, holder.ivPreview, FontAPI.FONT_PREVIEW_URL + item.preview, DirHelper.FONT_PREVIEW_DIR, item.preview, null);
            } else {
                holder.ivPreview.setImageResource(R.drawable.no_preview);
            }
        }
        return v;
    }
}
