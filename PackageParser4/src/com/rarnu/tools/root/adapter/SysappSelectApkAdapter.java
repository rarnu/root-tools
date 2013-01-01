package com.rarnu.tools.root.adapter;

import java.util.List;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.common.SysappSelectApkItem;
import com.rarnu.tools.root.holder.SysappSelectApkAdapterHolder;

public class SysappSelectApkAdapter extends BaseAdapter {

	// [region] field define
	private List<SysappSelectApkItem> list;
	private LayoutInflater inflate;

	// [/region]
	
	// [region] constructor
	public SysappSelectApkAdapter(List<SysappSelectApkItem> list,
			LayoutInflater inflater) {
		this.list = list;
		this.inflate = inflater;
	}
	
	// [/region]
	
	// [region] adapter

	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public SysappSelectApkItem getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		SysappSelectApkItem item = list.get(position);
		View view;
		if (convertView != null) {
			view = convertView;
		} else {
			view = inflate.inflate(R.layout.sysapp_file_item, parent, false);
		}

		SysappSelectApkAdapterHolder holder = (SysappSelectApkAdapterHolder) view.getTag();
		if (holder == null) {
			holder = new SysappSelectApkAdapterHolder();
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

			holder.tvFilename.setTextColor(Color.BLACK);

			switch (item.level) {
			case 0:
				holder.tvFilename.setTextColor(Color.RED);
				break;
			case 1:
				holder.tvFilename.setTextColor(Color.GREEN);
				break;
			case 2:
				holder.tvFilename.setTextColor(Color.BLUE);
				break;
			}

		}

		return view;
	}

	// [/region]
}
