package com.rarnu.vim.emotion.comp.calendar;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.vim.emotion.R;

public class MonthAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private CalendarDays days;

	public MonthAdapter(Context context, CalendarDays days) {
		this.inflater = LayoutInflater.from(context);
		this.days = days;
	}

	@Override
	public int getCount() {
		return days.lstDays.size();
	}

	@Override
	public Object getItem(int position) {
		return days.lstDays.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_day, parent, false);
		}
		DayHolder holder = (DayHolder) v.getTag();
		if (holder == null) {
			holder = new DayHolder();
			holder.dayImg = (ImageView) v.findViewById(R.id.day_img);
			holder.dayTitle = (TextView) v.findViewById(R.id.day_title);
			v.setTag(holder);
		}
		Day item = days.lstDays.get(position);
		if (item != null) {
			holder.dayTitle.setText(item.day == 0 ? "" : String
					.valueOf(item.day));
		}
		return v;
	}

}
