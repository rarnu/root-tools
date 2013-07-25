package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.utils.DownloadUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeImage;
import com.sbbs.me.android.consts.PathDefine;
import com.sbbs.me.android.utils.MiscUtils;

public class SbbsMeGalleryAdapter extends BaseAdapter<SbbsMeImage> {

	private int itemHeight;

	public SbbsMeGalleryAdapter(Context context, List<SbbsMeImage> list,
			int itemHeight) {
		super(context, list);
		this.itemHeight = itemHeight;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_gallery, parent, false);
			v.setLayoutParams(new AbsListView.LayoutParams(
					AbsListView.LayoutParams.MATCH_PARENT, itemHeight));
		}
		SbbsMeGalleryHolder holder = (SbbsMeGalleryHolder) v.getTag();
		if (holder == null) {
			holder = new SbbsMeGalleryHolder();
			holder.ivPic = (ImageView) v.findViewById(R.id.ivPic);
			holder.tvPicName = (TextView) v.findViewById(R.id.tvPicName);
			v.setTag(holder);
		}
		SbbsMeImage item = list.get(position);
		if (item != null) {
			holder.tvPicName.setText(item.Desc);
			DownloadUtils.downloadFileT(context, holder.ivPic, item.Src,
					PathDefine.ROOT_PATH,
					MiscUtils.extractFileNameFromURL(item.Src), null);
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMeImage item) {
		return "";
	}

}
