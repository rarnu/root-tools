package com.zoe.calendar.component;

import java.util.List;

import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.zoe.calendar.R;
import com.zoe.calendar.classes.CalendarItem;

public class MonthAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private CalendarDays days;
	private int rowHeight = 0;
	private Context context;

	private List<CalendarItem> listCalendar = null;

	public MonthAdapter(Context context, CalendarDays days, int rowHeight) {
		this.context = context;
		this.inflater = LayoutInflater.from(context);
		this.days = days;
		this.rowHeight = rowHeight;
	}

	public void setCalendarItem(List<CalendarItem> listCalendar) {
		this.listCalendar = listCalendar;
		Log.e("MonthAdapter", "setCalendarItem");
		this.notifyDataSetChanged();
	}

	private String getCalenderItem(int year, int month, int day) {
		String ret = "";
		if (listCalendar != null) {
			for (int i = 0; i < listCalendar.size(); i++) {
				if (listCalendar.get(i).year == year
						&& listCalendar.get(i).month == month
						&& listCalendar.get(i).day == day) {
					ret = listCalendar.get(i).text;
					break;
				}
			}
		}

		return ret;
	}

	@Override
	public int getCount() {
		return days.lstDays.size();
	}

	@Override
	public boolean isEnabled(int position) {
		return days.lstDays.get(position).day != 0;
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
			v = inflater.inflate(R.layout.item_month_day, parent, false);
			AbsListView.LayoutParams lp = (AbsListView.LayoutParams) v
					.getLayoutParams();
			lp.height = rowHeight;
			v.setLayoutParams(lp);
		}
		DayHolder holder = (DayHolder) v.getTag();
		if (holder == null) {
			holder = new DayHolder();
			holder.ivToday = (ImageView) v.findViewById(R.id.ivToday);
			holder.tvDayTitle = (TextView) v.findViewById(R.id.tvDaytitle);
			holder.tvLunar = (TextView) v.findViewById(R.id.tvLunar);
			// holder.ivClick = (ImageView) v.findViewById(R.id.ivClick);
			v.setTag(holder);
		}
		Day item = days.lstDays.get(position);
		if (item != null) {
			holder.tvDayTitle.setTextColor(Color.BLACK);
			holder.tvLunar.setTextColor(Color.BLACK);

			holder.tvDayTitle.setText(item.day == 0 ? "" : String
					.valueOf(item.day));
			holder.tvDayTitle.setTextColor(item.highlight ? 0xff0099CC
					: Color.BLACK);

			String calItemText = getCalenderItem(item.year, item.month,
					item.day);
			if (calItemText.equals("")) {
				holder.tvLunar.setText(item.chineseDay);
				holder.tvLunar.setTextColor(Color.BLACK);
			} else {
				holder.tvLunar.setText(calItemText);
				holder.tvLunar.setTextColor(0xff0099CC);
			}

			if (item.today) {
				holder.tvDayTitle.setTextColor(0xff009933);
				holder.tvLunar.setTextColor(0xff009933);

			} else {
				holder.ivToday.setImageDrawable(null);
			}

			holder.ivToday.setImageDrawable(item.selected ? context
					.getResources().getDrawable(R.drawable.today) : null);

		}
		return v;
	}

}
