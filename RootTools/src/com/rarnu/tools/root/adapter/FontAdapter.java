package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.FontAPI;
import com.rarnu.tools.root.common.FontItem;
import com.rarnu.tools.root.holder.FontHolder;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.ImageUtils;
import org.apache.http.protocol.HTTP;

import java.net.URLEncoder;
import java.util.List;

public class FontAdapter extends BaseAdapter<FontItem> {


    private static final int ROUND_RADIS = 8;

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
                try {
                    DownloadUtils.downloadFileT(context, holder.ivPreview, FontAPI.FONT_PREVIEW_URL + URLEncoder.encode(item.preview, HTTP.UTF_8), DirHelper.FONT_PREVIEW_DIR, item.preview, null, true, ROUND_RADIS);
                } catch (Exception e) {

                }
            } else {

                Bitmap bmp = ImageUtils.drawableToBitmap(context.getResources().getDrawable(R.drawable.no_preview));
                bmp = ImageUtils.roundedCornerBitmap(bmp, ROUND_RADIS);
                holder.ivPreview.setImageBitmap(bmp);
            }
        }
        return v;
    }
}
