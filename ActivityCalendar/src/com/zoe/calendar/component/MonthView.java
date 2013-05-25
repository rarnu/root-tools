package com.zoe.calendar.component;

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

	public void setOnDayClickListener(DayClickListener listener) {
		this.listener = listener;
	}

	private void init() {
		setNumColumns(7);
		setOnItemClickListener(this);
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
		if (listener != null) {
			listener.onDayClick(d);
		}
	}

}
