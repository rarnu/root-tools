package com.snda.root.memory.adapter;

import java.util.List;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.root.memory.Global;
import com.snda.root.memory.R;
import com.snda.root.memory.utils.ProcessInfo;

public class ProcessAdapter extends BaseAdapter {

	List<ProcessInfo> list;
	LayoutInflater inflater;

	public ProcessAdapter(List<ProcessInfo> list, LayoutInflater inflater) {
		this.list = list;
		this.inflater = inflater;
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
		ProcessInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.process_item, parent, false);
		} else {
			v = convertView;
		}
		ProcessHolder holder = (ProcessHolder) v.getTag();
		if (holder == null) {
			holder = new ProcessHolder();
			holder.item_icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.item_name = (TextView) v.findViewById(R.id.item_name);
			holder.item_memory_used = (TextView) v
					.findViewById(R.id.item_memory_used);
			holder.item_namespace = (TextView) v
					.findViewById(R.id.item_namespace);
			v.setTag(holder);
		}

		if (item != null) {
			if (item.appInfo == null) {
				holder.item_icon.setBackgroundDrawable(v.getResources().getDrawable(
						R.drawable.default_icon));
				holder.item_name.setText(item.NAME);
				
				holder.item_namespace.setText("");
			} else {
				holder.item_icon.setBackgroundDrawable(Global.pm.getApplicationIcon(item.appInfo));
				holder.item_name.setText(Global.pm.getApplicationLabel(item.appInfo));
				holder.item_namespace.setText(item.NAME);
			}
			holder.item_memory_used.setText(String.format("%dM", item.RSS));
		}
		return v;
	}
}
