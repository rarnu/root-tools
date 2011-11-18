package com.snda.root.busybox.adapter;

import java.util.List;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.snda.root.busybox.R;

public class BusyboxAdapter extends BaseAdapter {
	
	private LayoutInflater inflater;
	private List<BusyboxItem> list;
	
	public BusyboxAdapter(LayoutInflater inflater, List<BusyboxItem> list) {
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
		BusyboxItem item = list.get(position);
		
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.item, parent, false);
		} else {
			v = convertView;
		}
		
		BusyboxHolder holder = (BusyboxHolder) v.getTag();
		if (holder == null) {
			holder = new BusyboxHolder();
			holder.name = (TextView) v.findViewById(R.id.item_name);
		}
		
		if (item != null) {
			holder.name.setText(item.name);
		}
		
		return v;
	}

}
