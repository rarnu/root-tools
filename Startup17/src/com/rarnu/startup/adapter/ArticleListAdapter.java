package com.rarnu.startup.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.startup.R;
import com.rarnu.startup.pojo.ArticleListItem;

import java.util.List;

public class ArticleListAdapter extends BaseAdapter<ArticleListItem> {
    public ArticleListAdapter(Context context, List<ArticleListItem> list) {
        super(context, list);
    }

    @Override
    public String getValueText(ArticleListItem item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item_article_list, parent, false);
        }
        ArticleItemHolder holder = (ArticleItemHolder) v.getTag();
        if (holder == null) {
            holder = new ArticleItemHolder();
            holder.ivImage = (ImageView) v.findViewById(R.id.ivImage);
            holder.tvTitle = (TextView) v.findViewById(R.id.tvTitle);
            holder.tvDesc = (TextView) v.findViewById(R.id.tvDesc);
            holder.tvDatetime = (TextView) v.findViewById(R.id.tvDatetime);
            v.setTag(holder);
        }

        ArticleListItem item = list.get(position);
        if (item != null) {
            holder.tvTitle.setText(item.name);
            holder.tvDesc.setText(item.desc);
            holder.tvDatetime.setText(item.approved_time);
        }

        return v;
    }
}
