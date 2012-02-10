package com.snda.gyue.adapter;

import java.util.List;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Gallery;
import android.widget.ImageView;

import com.snda.gyue.GlobalInstance;
import com.snda.gyue.GyueConsts;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.network.NetFiles;
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

		ArticleItem item = mList.get(position);

		ImageView i = new ImageView(mContext);
		i.setScaleType(ImageView.ScaleType.FIT_CENTER);
		i.setLayoutParams(new Gallery.LayoutParams(GlobalInstance.metric.widthPixels - 8,
				(int) (260 * GlobalInstance.metric.widthPixels / 480)));

		Drawable d = ImageUtils.loadFullImage(mContext, GyueConsts.GYUE_DIR + item.getArticleImageLocalFileName());

		if (d != null) {
			i.setBackgroundDrawable(d);
		} else {
			NetFiles.doDownloadImageT(mContext, item.getArticleImageUrl(), item.getArticleImageLocalFileName(), i);
		}

		return i;
	}
}
