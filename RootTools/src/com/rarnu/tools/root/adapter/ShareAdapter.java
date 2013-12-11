package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.ShareItem;
import com.rarnu.tools.root.holder.ShareHolder;

import java.util.List;

public class ShareAdapter extends BaseAdapter<ShareItem> {

    public ShareAdapter(Context context, List<ShareItem> list) {
        super(context, list);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.share_item, parent, false);
        }
        ShareHolder holder = (ShareHolder) v.getTag();
        if (holder == null) {
            holder = new ShareHolder();
            holder.ivShareIcon = (ImageView) v.findViewById(R.id.ivShareIcon);
            holder.tvShareTitle = (TextView) v.findViewById(R.id.tvShareTitle);
            v.setTag(holder);
        }
        ShareItem item = list.get(position);
        if (item != null) {
            holder.ivShareIcon.setImageDrawable(item.icon);
            holder.tvShareTitle.setText(item.title);
        }
        return v;
    }

    @Override
    public String getValueText(ShareItem item) {
        return "";
    }

}
