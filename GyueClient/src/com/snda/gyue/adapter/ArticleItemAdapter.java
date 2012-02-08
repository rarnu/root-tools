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
			holder.articleDesc = (TextView) convertView
					.findViewById(R.id.article_desc);

			holder.articleDate = (TextView) convertView
					.findViewById(R.id.article_date);
			holder.articleImage = (ImageView) convertView
					.findViewById(R.id.article_image);

			convertView.setTag(holder);
		}

		holder = (ArticleItemHolder) convertView.getTag();

		ArticleItem item = list.get(position);
		if (item != null) {
			holder.articleTitle.setText(item.title);
			holder.articleDesc.setText(item.description);
			holder.articleDate.setText(item.date);
			
			if (item.articleImage == null) {
				holder.articleImage.setVisibility(View.GONE);
			} else {
				holder.articleImage.setVisibility(View.VISIBLE);
				holder.articleImage.setImageBitmap(item.articleImage);
			}
		}

		return convertView;
	}

}
