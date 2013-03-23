package com.rarnu.almanac;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.rarnu.almanac.Almanac.Result;

public class ItemAdapter extends BaseAdapter {

	private List<Result> list;
	private LayoutInflater inflater;

	public ItemAdapter(Context context, List<Result> list) {
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
			v = inflater.inflate(R.layout.item, parent, false);
		}
		ItemHolder holder = (ItemHolder) v.getTag();
		if (holder == null) {
			holder = new ItemHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			holder.tvDesc = (TextView) v.findViewById(R.id.tvDesc);
			v.setTag(holder);
		}
		Result item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item.name);
			holder.tvDesc.setText(item.desc);

		}
		return v;
	}

}
