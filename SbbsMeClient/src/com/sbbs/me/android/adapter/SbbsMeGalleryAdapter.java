package com.sbbs.me.android.adapter;

import java.util.List;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.AbsListView;
import android.widget.ImageView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.utils.DownloadUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeImage;
import com.sbbs.me.android.consts.PathDefine;

public class SbbsMeGalleryAdapter extends BaseAdapter<SbbsMeImage> {

	private int itemHeight;
	private boolean isEditMode = false;
	Animation animation = null;
	BitmapFactory.Options bop = null;

	public SbbsMeGalleryAdapter(Context context, List<SbbsMeImage> list,
			int itemHeight) {
		super(context, list);
		this.itemHeight = itemHeight;
		animation = AnimationUtils
				.loadAnimation(context, R.animator.anim_shake);
		bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;
	}

	public void setEditMode(boolean edit) {
		this.isEditMode = edit;
		this.notifyDataSetChanged();
	}

	public boolean getEditMode() {
		return isEditMode;
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
			holder.ivDelete = (ImageView) v.findViewById(R.id.ivDelete);
			v.setTag(holder);
		}
		SbbsMeImage item = list.get(position);
		if (item != null) {
			DownloadUtils.downloadFileT(context, holder.ivPic,
					SbbsMeAPI.ROOT_URL + item.URL, PathDefine.ROOT_PATH,
					item.FileName, null, bop);
			holder.ivDelete
					.setVisibility(isEditMode ? View.VISIBLE : View.GONE);
			holder.ivPic.setAnimation(isEditMode ? animation : null);
		}
		return v;
	}

	@Override
	public String getValueText(SbbsMeImage item) {
		return "";
	}

}
