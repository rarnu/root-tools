package com.snda.root.sapp.manager.adapter;

import java.util.List;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.root.sapp.manager.GlobalInstance;
import com.snda.root.sapp.manager.R;

public class AppAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<AppInfo> list;

	public AppAdapter(LayoutInflater inflater, List<AppInfo> list) {
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
		AppInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.app_item, parent, false);
		} else {
			v = convertView;
		}
		AppAdapterHolder holder = (AppAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new AppAdapterHolder();
			holder.icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.name = (TextView) v.findViewById(R.id.item_name);
			holder.path = (TextView) v.findViewById(R.id.item_path);
			v.setTag(holder);
		}

		if (item != null) {
			holder.icon.setBackgroundDrawable(GlobalInstance.pm
					.getApplicationIcon(item.info));
			holder.name.setText(GlobalInstance.pm
					.getApplicationLabel(item.info));
			holder.path.setText(item.info.sourceDir);

			holder.name.setTextColor(Color.WHITE);

			if (GlobalInstance.colorLevel) {
				switch (item.level) {
				case 0:
					holder.name.setTextColor(Color.RED);
					break;
				case 1:
					holder.name.setTextColor(Color.GREEN);
					break;
				}
			}
		}

		return v;
	}

}
