package com.rarnu.vim.emotion.comp.calendar;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.widget.GridView;

public class MonthView extends GridView {

	private CalendarDays days;
	private MonthAdapter adapter;

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

	private void init() {
		setNumColumns(7);
	}

	@Override
	public boolean dispatchTouchEvent(MotionEvent ev) {
		if (ev.getAction() == MotionEvent.ACTION_MOVE) {
			return true;
		}
		return super.dispatchTouchEvent(ev);
	}

	public void setDays(CalendarDays days) {
		this.days = days;
		adapter = new MonthAdapter(getContext(), days);
		setAdapter(adapter);
	}

	public CalendarDays getDays() {
		return days;
	}
}
