package com.zoe.calendar.component;

import com.zoe.calendar.R;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;

public class MonthView extends GridView implements OnItemClickListener {

	private CalendarDays days;
	private MonthAdapter adapter;

	private DayClickListener listener;
	private int monthIndex = 0;

	public MonthView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public MonthView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public MonthView(Context context) {
		super(context);
		init();
	}

	public void setMonthIndex(int index) {
		monthIndex = index;
	}

	public void setOnDayClickListener(DayClickListener listener) {
		this.listener = listener;
	}

	private void init() {
		setNumColumns(7);
		setOnItemClickListener(this);
		setSelector(R.drawable.grid_style);
	}

	@Override
	public boolean dispatchTouchEvent(MotionEvent ev) {
		if (ev.getAction() == MotionEvent.ACTION_MOVE) {
			return true;
		}
		return super.dispatchTouchEvent(ev);
	}

	public void setDays(CalendarDays days, int height) {
		this.days = days;
		adapter = new MonthAdapter(getContext(), days, height);
		setAdapter(adapter);
	}

	public CalendarDays getDays() {
		return days;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
		Day d = days.lstDays.get(position);
		for (int i = 0; i < days.lstDays.size(); i++) {
			days.lstDays.get(i).selected = false;
		}
		days.lstDays.get(position).selected = true;
		adapter.notifyDataSetChanged();
		if (listener != null) {
			listener.onDayClick(monthIndex, position, d);
		}
	}

	public void changeSelection(final int index, final int position) {

		for (int i = 0; i < days.lstDays.size(); i++) {
			if (index != monthIndex) {
				days.lstDays.get(i).selected = false;
			} else {
				days.lstDays.get(i).selected = (i == position);
			}
		}
		adapter.notifyDataSetChanged();

	}

}
