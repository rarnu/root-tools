package com.rarnu.zoe.loving.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.zoe.loving.R;

public class ImageAdapter extends BaseAdapter {

	private Context context;
	private List<Integer> list;
	private LayoutInflater inflater;

	public ImageAdapter(Context context, List<Integer> list) {
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

			((ImageView) v.findViewById(R.id.imgItem))
					.setBackgroundDrawable(context.getResources().getDrawable(
							R.drawable.test));

		}

		return v;
	}

}
