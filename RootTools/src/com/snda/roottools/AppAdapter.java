package com.snda.roottools;

import java.util.List;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

public class AppAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<SndaRootApplication> list;

	public AppAdapter(LayoutInflater inflater, List<SndaRootApplication> list) {
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
		SndaRootApplication item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.item, parent, false);
		} else {
			v = convertView;
		}
		AppAdapterHolder holder = (AppAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new AppAdapterHolder();
			holder.icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.name = (TextView) v.findViewById(R.id.item_name);
			holder.version = (TextView) v.findViewById(R.id.item_version);
			v.setTag(holder);
		}

		if (item != null) {
			holder.icon.setBackgroundDrawable(item.icon);
			holder.name.setText(item.name);
			holder.version.setText(item.version);
		}

		return v;
	}

}
