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

public class SelectApkAdapter extends BaseAdapter {

	private List<SelectApkItem> list;
	private LayoutInflater inflate;

	public SelectApkAdapter(List<SelectApkItem> list, LayoutInflater inflater) {
		this.list = list;
		this.inflate = inflater;
	}

	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public SelectApkItem getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		SelectApkItem item = list.get(position);
		View view;
		if (convertView != null) {
			view = convertView;
		} else {
			view = inflate.inflate(R.layout.file_item, parent, false);
		}

		SelectApkHolder holder = (SelectApkHolder) view.getTag();
		if (holder == null) {
			holder = new SelectApkHolder();
			holder.imgIcon = (ImageView) view.findViewById(R.id.img_icon);
			holder.tvFilename = (TextView) view.findViewById(R.id.tv_filename);
			view.setTag(holder);
		}

		if (item != null) {
			if (item.icon == 1) {
				holder.imgIcon.setBackgroundDrawable(view.getResources()
						.getDrawable(R.drawable.folder));
			} else {
				holder.imgIcon.setBackgroundDrawable(item.iconImg);
			}
			holder.tvFilename.setText(item.filename);

			holder.tvFilename.setTextColor(Color.WHITE);
			if (GlobalInstance.colorLevel) {
				switch (item.level) {
				case 0:
					holder.tvFilename.setTextColor(Color.RED);
					break;
				case 1:
					holder.tvFilename.setTextColor(Color.GREEN);
					break;
				}
			}
		}

		return view;
	}

}
