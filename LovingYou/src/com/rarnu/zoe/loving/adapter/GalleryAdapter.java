package com.rarnu.zoe.loving.adapter;

import java.util.List;

import com.rarnu.zoe.loving.R;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;

public class GalleryAdapter extends BaseAdapter {

	private Context context;
	private List<Integer> list;
	private LayoutInflater inflater;

	private BitmapFactory.Options bop = null;

	public GalleryAdapter(Context context, List<Integer> list) {
		this.context = context;
		this.list = list;
		this.inflater = LayoutInflater.from(context);

		bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;
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
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_image, parent, false);
		}
		GalleryHolder holder = (GalleryHolder) v.getTag();
		if (holder == null) {
			holder = new GalleryHolder();
			holder.imgItem = (ImageView) v.findViewById(R.id.imgItem);
			v.setTag(holder);
		}
		Integer item = list.get(position);
		if (item != null) {
			holder.imgItem.setImageBitmap(BitmapFactory.decodeResource(
					context.getResources(), item, bop));
		}
		return v;
	}

}
