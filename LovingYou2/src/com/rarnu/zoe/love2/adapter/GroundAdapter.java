package com.rarnu.zoe.love2.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.zoe.love2.R;
import com.rarnu.zoe.love2.common.GroundInfo;
import com.rarnu.zoe.love2.utils.DownloadUtils;

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
			holder.tvDay = (TextView) v.findViewById(R.id.tvDay);
			v.setTag(holder);
		}
		GroundInfo item = list.get(position);
		if (item != null) {
			DownloadUtils.downloadFileT(context, item, holder.imgGround, 0, null);
			holder.tvDay.setText(item.txt);
		}
		return v;
	}

}
