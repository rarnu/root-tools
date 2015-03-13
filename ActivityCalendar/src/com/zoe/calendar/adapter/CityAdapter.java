package com.zoe.calendar.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.CityItem;

public class CityAdapter extends BaseAdapter<CityItem> {

	public CityAdapter(Context context, List<CityItem> list) {
		super(context, list);
	}
	
	@Override
	public boolean isEnabled(int position) {
		return !list.get(position).pinyin.trim().equals("");
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		CityItem item = list.get(position);
		if (item.pinyin.equals("")) {
			v = inflater.inflate(R.layout.item_city_section, parent, false);
		} else {
			v = inflater.inflate(R.layout.item_city_select, parent, false);
		}
		if (v instanceof TextView) {
			((TextView) v).setText(item.name);
		} else {
			CityHolder holder = (CityHolder) v.getTag();
			if (holder == null) {
				holder = new CityHolder();
				holder.tvCityValue = (TextView) v
						.findViewById(R.id.tvCityValue);
				v.setTag(holder);
			}
			if (item != null) {
				holder.tvCityValue.setText(item.name);
			}
		}
		return v;
	}

	@Override
	public String getValueText(CityItem item) {
		if (item.pinyin.equals("")) {
			return "";
		} else {
			return item.name + item.pinyin.toLowerCase();
		}
	}

}
