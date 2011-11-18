package com.snda.roottools;

import java.util.List;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

public class ToolAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<ToolInfo> list;

	public ToolAdapter(LayoutInflater inflater, List<ToolInfo> list) {
		this.inflater = inflater;
		this.list = list;
	}

	public int getCount() {
		return list.size();
	}

	public Object getItem(int arg0) {
		return list.get(arg0);
	}

	public long getItemId(int arg0) {
		return arg0;
	}

	public View getView(int position, View convertView, ViewGroup parent) {
		ToolInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.tool_item, parent, false);
		} else {
			v = convertView;
		}
		ToolAdapterHolder holder = (ToolAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new ToolAdapterHolder();
			holder.status = (TextView) v.findViewById(R.id.tool_status);
			holder.name = (TextView) v.findViewById(R.id.tool_name);
			holder.desc = (TextView) v.findViewById(R.id.tool_desc);
			v.setTag(holder);
		}

		if (item != null) {
			holder.name.setText(item.name + " (" + item.version + ")");
			holder.desc.setText(item.desc);
			switch (MainActivity.isToolInstalled(item.packageName, item.version_code)) {
			case 0:
				holder.status.setText(R.string.st_not_installed);
				holder.status.setTextColor(Color.GRAY);
				break;
			case 1:
				holder.status.setText(R.string.st_installed);
				holder.status.setTextColor(Color.GREEN);
				break;
			case 2:
				holder.status.setText(R.string.st_updatable);
				holder.status.setTextColor(Color.YELLOW);
				break;
			}
			
		}

		return v;
	}

}
