package com.rarnu.adcenter.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.adcenter.R;
import com.rarnu.devlib.base.adapter.BaseAdapter;

public class LeftMenuItemAdapter extends BaseAdapter<String> {

	public LeftMenuItemAdapter(Context context, List<String> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_left_menu, parent, false);
		}
		LeftMenuItemHolder holder = (LeftMenuItemHolder) v.getTag();
		if (holder == null) {
			holder = new LeftMenuItemHolder();
			holder.tvName = (TextView) v.findViewById(R.id.tvName);
			v.setTag(holder);
		}
		String item = list.get(position);
		if (item != null) {
			holder.tvName.setText(item);
		}
		return v;
	}

	@Override
	public String getValueText(String item) {
		return "";
	}

}
