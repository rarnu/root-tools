package com.rarnu.fontter.adapter;

import android.content.Context;
import android.graphics.Color;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.fontter.R;
import com.rarnu.fontter.api.FontItem;

import java.util.List;

public class FontAdapter extends BaseAdapter<FontItem> {

    private Handler hDownload;
    private boolean isDownloading;

    public FontAdapter(Context context, List<FontItem> list, Handler hDownload) {
        super(context, list);
        this.hDownload = hDownload;
    }

    public void setDownloading(boolean d) {
        isDownloading = d;
        this.notifyDataSetChanged();
    }

    @Override
    public String getValueText(FontItem item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.font_item, parent, false);
        }
        FontHolder holder = (FontHolder) v.getTag();
        if (holder == null) {
            holder = new FontHolder();
            holder.tvName = (TextView) v.findViewById(R.id.tvName);
            holder.tvStatus = (TextView) v.findViewById(R.id.tvState);
            holder.btnDownload = (Button) v.findViewById(R.id.btnDownload);
            v.setTag(holder);
        }
        final FontItem item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.name);
            holder.tvName.setTextColor(item.inUse ? context.getResources().getColor(R.color.greenyellow) : Color.WHITE);
            holder.tvStatus.setText(item.isDownloaded ? context.getString(R.string.font_downloaded) : "");
            if (isDownloading) {
                holder.btnDownload.setVisibility(View.GONE);
            } else {
                holder.btnDownload.setVisibility(item.isDownloaded ? View.GONE : View.VISIBLE);
                holder.btnDownload.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        if (hDownload != null) {
                            Message msg = new Message();
                            msg.obj = item.fileName;
                            hDownload.sendMessage(msg);
                        }
                    }
                });
            }
        }
        return v;
    }
}
