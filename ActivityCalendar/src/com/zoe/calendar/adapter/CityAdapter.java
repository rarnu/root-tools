package com.zoe.calendar.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.CityItem;

public class CityAdapter extends BaseAdapter<CityItem> {

	public CityAdapter(Context context, List<CityItem> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_city_select, parent, false);
		}
		CityHolder holder = (CityHolder) v.getTag();
		if (holder == null) {
			holder = new CityHolder();
			holder.tvCityValue = (TextView) v.findViewById(R.id.tvCityValue);
			v.setTag(holder);
		}
		CityItem item = list.get(position);
		if (item != null) {
			holder.tvCityValue.setText(item.name);
		}
		return v;
	}

	@Override
	public String getValueText(CityItem item) {
		return item.name + item.pinyin.toLowerCase();
	}

}
