package com.zoe.calendar.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.LeftMenuItem;

public class LeftMenuAdapter extends BaseAdapter<LeftMenuItem> {

	public LeftMenuAdapter(Context context, List<LeftMenuItem> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_left_menu, parent, false);
		}
		LeftMenuHolder holder =(LeftMenuHolder) v.getTag();
		if (holder == null) {
			holder = new LeftMenuHolder();
			holder.ivIcon = (ImageView) v.findViewById(R.id.ivIcon);
			holder.tvTitle = (TextView) v.findViewById(R.id.tvTitle);
			v.setTag(holder);
		}
		LeftMenuItem item = list.get(position);
		if (item != null) {
			holder.ivIcon.setImageResource(item.icon);
			holder.tvTitle.setText(item.title);
		}
 		return v;
	}

	@Override
	public String getValueText(LeftMenuItem item) {
		return "";
	}

}
