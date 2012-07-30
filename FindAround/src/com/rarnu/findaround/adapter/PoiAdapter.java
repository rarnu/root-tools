package com.rarnu.findaround.adapter;

import java.util.List;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.baidu.mapapi.MKPoiInfo;
import com.rarnu.findaround.R;

public class PoiAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<MKPoiInfo> list;

	public PoiAdapter(LayoutInflater inflater, List<MKPoiInfo> list) {
		this.inflater = inflater;
		this.list = list;
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
			v = inflater.inflate(R.layout.poi_item, parent, false);
		}
		PoiHolder holder = (PoiHolder) v.getTag();
		if (holder == null) {
			holder = new PoiHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			holder.tvAddress = (TextView) v.findViewById(R.id.tvAddress);
			v.setTag(holder);
		}
		MKPoiInfo item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item.name);
			holder.tvAddress.setText(item.address);
		}
		return v;
	}

}
