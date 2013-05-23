package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.SysappSelectApkItem;
import com.rarnu.tools.root.holder.SysappSelectApkAdapterHolder;
import com.rarnu.utils.DrawableUtils;

public class SysappSelectApkAdapter extends BaseAdapter<SysappSelectApkItem> {

	public SysappSelectApkAdapter(Context context,
			List<SysappSelectApkItem> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		SysappSelectApkItem item = list.get(position);
		View view;
		if (convertView != null) {
			view = convertView;
		} else {
			view = inflater.inflate(R.layout.sysapp_file_item, parent, false);
		}

		SysappSelectApkAdapterHolder holder = (SysappSelectApkAdapterHolder) view
				.getTag();
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

			holder.tvFilename.setTextColor(DrawableUtils
					.getTextColorPrimary(context));

			switch (item.level) {
			case 0:
				holder.tvFilename.setTextColor(Color.RED);
				break;
			case 1:
				holder.tvFilename.setTextColor(Color.GREEN);
				break;
			case 2:
				holder.tvFilename.setTextColor(0xFF6495ED);
				break;
			}

		}

		return view;
	}

	@Override
	public String getValueText(SysappSelectApkItem item) {
		return item.filename;
	}

}
