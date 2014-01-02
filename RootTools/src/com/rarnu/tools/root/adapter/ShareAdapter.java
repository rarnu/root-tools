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

    private int[] share_item_icons = new int[]{
            R.drawable.share1, R.drawable.share2, R.drawable.share3,
            R.drawable.share4, R.drawable.share5, R.drawable.share6,
            R.drawable.share7, R.drawable.share8, R.drawable.share9
    };
    private int[] share_item_na_icons = new int[]{
            R.drawable.share1_na, R.drawable.share2_na, R.drawable.share3_na,
            R.drawable.share4_na, R.drawable.share5_na, R.drawable.share6_na,
            R.drawable.share7_na, R.drawable.share8_na, R.drawable.share9_na,
    };

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
            holder.ivShareIcon.setImageResource(item.installed ? share_item_icons[item.id] : share_item_na_icons[item.id]);
            holder.tvShareTitle.setText(item.title);
        }
        return v;
    }

    @Override
    public String getValueText(ShareItem item) {
        return "";
    }

}
