package com.snda.gyue.adapter;

import java.util.List;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.gyue.R;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.holder.ArticleItemHolder;

public class ArticleItemAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<ArticleItem> list;

	public ArticleItemAdapter(LayoutInflater inflater, List<ArticleItem> list) {
		this.inflater = inflater;
		this.list = list;
	}

	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public Object getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {

		ArticleItemHolder holder;
		if (convertView == null) {
			convertView = inflater
					.inflate(R.layout.article_item, parent, false);
			holder = new ArticleItemHolder();
			holder.articleTitle = (TextView) convertView
					.findViewById(R.id.article_title);
			holder.articleDateTag = (TextView) convertView
					.findViewById(R.id.article_date_tag);

			holder.articleRecommand = (ImageView) convertView
					.findViewById(R.id.article_recommand);
			holder.articleHot = (ImageView) convertView
					.findViewById(R.id.article_hot);

			convertView.setTag(holder);
		}

		holder = (ArticleItemHolder) convertView.getTag();

		ArticleItem item = list.get(position);
		if (item != null) {
			holder.articleTitle.setText(item.getTitle());
			holder.articleDateTag.setText(String.format(convertView
					.getResources().getString(R.string.item_tag), item
					.getDatetime(), item.getTag()));
			holder.articleRecommand
					.setVisibility(item.isRecommand() ? View.VISIBLE
							: View.GONE);
			holder.articleHot.setVisibility(item.isHot() ? View.VISIBLE
					: View.GONE);
		}

		return convertView;
	}

}
