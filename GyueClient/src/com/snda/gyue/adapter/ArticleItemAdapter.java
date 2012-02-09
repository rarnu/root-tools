package com.snda.gyue.adapter;

import java.io.File;
import java.util.List;

import android.text.Html;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.gyue.GyueConsts;
import com.snda.gyue.R;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.holder.ArticleItemHolder;
import com.snda.gyue.network.NetFiles;
import com.snda.gyue.utils.ImageUtils;

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
			convertView = inflater.inflate(R.layout.article_item, parent, false);
			holder = new ArticleItemHolder();
			holder.articleTitle = (TextView) convertView.findViewById(R.id.article_title);
			holder.articleDesc = (TextView) convertView.findViewById(R.id.article_desc);

			holder.articleDate = (TextView) convertView.findViewById(R.id.article_date);
			holder.articleImage = (ImageView) convertView.findViewById(R.id.article_image);

			convertView.setTag(holder);
		}

		holder = (ArticleItemHolder) convertView.getTag();

		ArticleItem item = list.get(position);
		if (item != null) {
			holder.articleTitle.setText(item.getTitle());
			holder.articleDesc.setText(Html.fromHtml(item.getDescription()));
			holder.articleDate.setText(item.getDate());

			if ((item.getArticleImageUrl() == null) || item.getArticleImageUrl().equals("")) {
				holder.articleImage.setVisibility(View.GONE);
			} else {
				holder.articleImage.setVisibility(View.VISIBLE);
				File img = new File(GyueConsts.GYUE_DIR + item.getArticleImageLocalFileName());
				if (img.exists()) {
					holder.articleImage.setBackgroundDrawable(ImageUtils.loadItemImage(convertView.getContext(),
							GyueConsts.GYUE_DIR + item.getArticleImageLocalFileName()));

				} else {
					NetFiles.doDownloadImageT(convertView.getContext(), item.getArticleImageUrl(), item.getArticleImageLocalFileName(),
							holder.articleImage);
				}
			}
		}

		return convertView;
	}
}
