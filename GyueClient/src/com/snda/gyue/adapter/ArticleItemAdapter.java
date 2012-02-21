package com.snda.gyue.adapter;

import java.io.File;
import java.util.List;

import android.text.Html;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.BaseAdapter;
import android.widget.Gallery;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;

import com.snda.gyue.GlobalInstance;
import com.snda.gyue.GyueConsts;
import com.snda.gyue.R;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.holder.ArticleItemHolder;
import com.snda.gyue.network.NetFiles;
import com.snda.gyue.utils.ImageUtils;

public class ArticleItemAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<ArticleItem> list;
	private ListView listview;
	private Gallery gallery;
	private int articleId;

	public ArticleItemAdapter(LayoutInflater inflater, List<ArticleItem> list, ListView listview, Gallery gallery, int articleId) {
		this.inflater = inflater;
		this.list = list;
		this.listview = listview;
		this.gallery = gallery;
		this.articleId = articleId;
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

		ArticleItem item = list.get(position);

		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.article_item, parent, false);
		} else {
			v = convertView;
		}

		ArticleItemHolder holder = (ArticleItemHolder) v.getTag();

		if (holder == null) {
			holder = new ArticleItemHolder();
			holder.articleTitle = (TextView) v.findViewById(R.id.article_title);
			holder.articleDesc = (TextView) v.findViewById(R.id.article_desc);

			holder.articleDate = (TextView) v.findViewById(R.id.article_date);
			holder.articleImage = (ImageView) v.findViewById(R.id.article_image);

			v.setTag(holder);
		}

		if (item != null) {

			holder.articleDate.setVisibility(View.VISIBLE);
			holder.articleDesc.setVisibility(View.VISIBLE);
			holder.articleImage.setVisibility(View.VISIBLE);

			if (item.getTitle().equals("0")) {
				switch (articleId) {
				case 54:
					holder.articleTitle.setText(R.string.more_func1_detail);
					break;
				case 13:
					holder.articleTitle.setText(R.string.more_func2_detail);
					break;
				case 11:
					holder.articleTitle.setText(R.string.more_func3_detail);
					break;
				case 12:
					holder.articleTitle.setText(R.string.more_func4_detail);
					break;
				}
				
				holder.articleDate.setVisibility(View.GONE);
				holder.articleDesc.setVisibility(View.GONE);
				holder.articleImage.setVisibility(View.GONE);

				RelativeLayout.LayoutParams lpMore = (RelativeLayout.LayoutParams) holder.articleTitle
						.getLayoutParams();
				lpMore.width = LayoutParams.WRAP_CONTENT;
				lpMore.height = LayoutParams.MATCH_PARENT;
				lpMore.addRule(RelativeLayout.CENTER_HORIZONTAL, 1);
				holder.articleTitle.setLayoutParams(lpMore);
				holder.articleTitle.setGravity(Gravity.CENTER);

				AbsListView.LayoutParams lpV = (AbsListView.LayoutParams) v.getLayoutParams();
				lpV.height = ImageUtils.dipToPx(GlobalInstance.density, 48);
				v.setLayoutParams(lpV);
			} else {
				holder.articleTitle.setText(item.getTitle());
				holder.articleDesc.setText(Html.fromHtml(item.getDescription()));
				holder.articleDate.setText(item.getDate().substring(5));

				RelativeLayout.LayoutParams lpMore = (RelativeLayout.LayoutParams) holder.articleTitle
						.getLayoutParams();
				lpMore.width = LayoutParams.MATCH_PARENT;
				lpMore.height = ImageUtils.dipToPx(GlobalInstance.density, 24);
				holder.articleTitle.setLayoutParams(lpMore);
				holder.articleTitle.setGravity(Gravity.LEFT | Gravity.CENTER_VERTICAL);

				if ((item.getArticleImageUrl() == null) || item.getArticleImageUrl().equals("")) {
					holder.articleImage.setVisibility(View.GONE);
				} else {
					holder.articleImage.setVisibility(View.VISIBLE);
					File img = new File(GyueConsts.GYUE_DIR + item.getArticleImageLocalFileName());
					if (img.exists()) {
						holder.articleImage.setBackgroundDrawable(ImageUtils.loadItemImage(v.getContext(),
								GyueConsts.GYUE_DIR + item.getArticleImageLocalFileName()));

					} else {
						NetFiles.doDownloadImageT(v.getContext(), item.getArticleImageUrl(),
								item.getArticleImageLocalFileName(), holder.articleImage, listview, gallery);
					}
				}
				AbsListView.LayoutParams lpV = (AbsListView.LayoutParams) v.getLayoutParams();
				lpV.height = ImageUtils.dipToPx(GlobalInstance.density, 96);
				v.setLayoutParams(lpV);
			}
		}

		return v;
	}
}
