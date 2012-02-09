package com.snda.gyue.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Gallery;
import android.widget.ImageView;

import com.snda.gyue.GlobalInstance;
import com.snda.gyue.GyueConsts;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.utils.ImageUtils;

public class ImageAdapter extends BaseAdapter {

	private Context mContext;
	private List<ArticleItem> mList;

	public ImageAdapter(Context c, List<ArticleItem> list) {
		mContext = c;
		mList = list;
	}

	public int getCount() {
		return mList.size();
	}

	public Object getItem(int position) {
		return mList.get(position);
	}

	public long getItemId(int position) {
		return position;
	}

	public View getView(int position, View convertView, ViewGroup parent) {
		ImageView i = new ImageView(mContext);
		i.setBackgroundDrawable(ImageUtils.loadFullImage(mContext, GyueConsts.GYUE_DIR + mList.get(position).getArticleImageLocalFileName()));
		i.setScaleType(ImageView.ScaleType.FIT_CENTER);
		i.setLayoutParams(new Gallery.LayoutParams(GlobalInstance.metric.widthPixels - 8, (int) (260 * GlobalInstance.metric.widthPixels / 480)));
		return i;
	}
}
