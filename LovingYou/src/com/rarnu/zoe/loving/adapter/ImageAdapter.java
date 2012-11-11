package com.rarnu.zoe.loving.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.common.ImageInfo;

public class ImageAdapter extends BaseAdapter {

	private Context context;
	private List<ImageInfo> list;
	private LayoutInflater inflater;

	public ImageAdapter(Context context, List<ImageInfo> list) {
		this.context = context;
		this.list = list;
		this.inflater = LayoutInflater.from(context);
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
		
		ImageHolder holder = (ImageHolder) v.getTag();
		if (holder == null) {
			holder = new ImageHolder();
			holder.tvItem = (TextView) v.findViewById(R.id.tvItem);
			holder.imgItem = (ImageView) v.findViewById(R.id.imgItem);
			v.setTag(holder);
		}
		
		ImageInfo item = list.get(position);
		if (item != null) {
			holder.imgItem.setVisibility(item.showImage ? View.VISIBLE: View.GONE);
			holder.tvItem.setVisibility(item.showImage ? View.GONE: View.VISIBLE);
			holder.imgItem.setImageDrawable(context.getResources().getDrawable(item.image));
			holder.tvItem.setText(item.text);
		}

		return v;
	}

}
