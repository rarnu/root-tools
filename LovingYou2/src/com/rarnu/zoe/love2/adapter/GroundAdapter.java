package com.rarnu.zoe.love2.adapter;

import java.util.List;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.zoe.love2.R;
import com.rarnu.zoe.love2.common.GroundInfo;

public class GroundAdapter extends BaseAdapter {

	private Context context;
	private List<GroundInfo> list;
	private LayoutInflater inflater;

	public GroundAdapter(Context context, List<GroundInfo> list) {
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
			v = inflater.inflate(R.layout.item_ground, parent, false);
		}
		GroundHolder holder = (GroundHolder) v.getTag();
		if (holder == null) {
			holder = new GroundHolder();
			holder.imgGround = (ImageView) v.findViewById(R.id.imgGround);
			holder.imgHeart = (ImageView) v.findViewById(R.id.imgHeart);
			holder.tvDay = (TextView) v.findViewById(R.id.tvDay);
			v.setTag(holder);
		}
		GroundInfo item = list.get(position);
		if (item != null) {
			holder.imgGround
					.setImageBitmap(BitmapFactory.decodeFile(item.path));
			holder.imgHeart
					.setImageResource(item.fav == 0 ? R.drawable.ground_unchecked
							: R.drawable.ground_checked);
			holder.tvDay.setText(String.format(
					context.getString(R.string.day_pfmt), item.day));
		}
		return v;
	}

}
