package com.snda.gyue.adapter;

import java.util.List;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.snda.gyue.GlobalInstance;
import com.snda.gyue.GyueConsts;
import com.snda.gyue.R;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.holder.GalleryHolder;
import com.snda.gyue.network.NetFiles;
import com.snda.gyue.utils.ImageUtils;

public class ImageAdapter extends BaseAdapter {

	private Context mContext;
	private List<ArticleItem> mList;
	private LayoutInflater inflater;

	public ImageAdapter(Context c, LayoutInflater inflater, List<ArticleItem> list) {
		mContext = c;
		mList = list;
		this.inflater = inflater;
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

		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.gallery_item, parent, false);
		} else {
			v = convertView;
		}

		GalleryHolder holder = (GalleryHolder) v.getTag();
		if (holder == null) {
			holder = new GalleryHolder();
			holder.galleryPicture = (ImageView) v.findViewById(R.id.gallery_picture);
			holder.galleryText = (TextView) v.findViewById(R.id.gallery_text);
			v.setTag(holder);
		}

		if (item != null) {

			holder.galleryPicture.setLayoutParams(new RelativeLayout.LayoutParams(GlobalInstance.metric.widthPixels - 8,
					(int) (260 * GlobalInstance.metric.widthPixels / 480)));

			Drawable d = ImageUtils.loadFullImage(mContext, GyueConsts.GYUE_DIR + item.getArticleImageLocalFileName());

			if (d != null) {
				holder.galleryPicture.setBackgroundDrawable(d);
			} else {
				NetFiles.doDownloadImageT(mContext, item.getArticleImageUrl(), item.getArticleImageLocalFileName(), holder.galleryPicture);
			}
			
			holder.galleryText.setText(item.getTitle());

		}

		return v;
	}
}
