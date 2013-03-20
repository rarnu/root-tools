package com.rarnu.kevin.medic.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseAdapter;
import com.rarnu.kevin.medic.R;

public class DataItemAdapter extends BaseAdapter<String> {

	public DataItemAdapter(Context context, List<String> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_data, parent, false);
		}
		DataItemHolder holder = (DataItemHolder) v.getTag();
		if (holder == null) {
			holder = new DataItemHolder();
			holder.tvItemTitle = (TextView) v.findViewById(R.id.tvItemTitle);
			v.setTag(holder);
		}
		String item = list.get(position);
		if (item != null) {
			holder.tvItemTitle.setText(item);
		}
		return v;
	}

	@Override
	public String getValueText(String item) {
		return "";
	}

}
