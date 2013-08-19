package com.rarnu.battery.notifier.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.rarnu.battery.notifier.R;
import com.rarnu.battery.notifier.classes.BatteryState;

public class BattaryAdapter extends BaseAdapter {

	private List<BatteryState> list;
	private LayoutInflater inflater;

	public BattaryAdapter(Context context, List<BatteryState> list) {
		this.inflater = LayoutInflater.from(context);
		this.list = list;
	}
	
	public void setNewList(List<BatteryState> list) {
		this.list = list;
		this.notifyDataSetChanged();
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
			v = inflater.inflate(R.layout.item_battery, parent, false);
		}
		BatteryHolder holder = (BatteryHolder) v.getTag();
		if (holder == null) {
			holder = new BatteryHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			holder.tvState = (TextView) v.findViewById(R.id.tvStatus);
			v.setTag(holder);
		}
		BatteryState item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item.itemName);
			holder.tvState.setText(item.stateName);
		}
		return v;
	}

}
