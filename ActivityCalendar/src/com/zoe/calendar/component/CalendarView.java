package com.zoe.calendar.component;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.RelativeLayout;

import com.rarnu.devlib.component.HScrollLayout;
import com.rarnu.devlib.component.intf.OnScreenChangeListener;

public class CalendarView extends HScrollLayout implements
		OnScreenChangeListener, DayClickListener {

	public interface OnCalendarChange {
		void calendarChanged(CalendarDays days);
	}

	List<CalendarDays> listDays = new ArrayList<CalendarDays>();
	MonthView mCurrent, mPrior, mNext;
	OnCalendarChange calendarChange;

	OnClickListener clickListener;
	DayClickListener dayClickListener;

	public CalendarView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public CalendarView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	private void init() {
		setOnScreenChangeListener(this);
	}

	public void setDayClickListener(DayClickListener listener) {
		this.dayClickListener = listener;
	}

	private MonthView addMonth(CalendarDays days, int height, boolean first) {
		RelativeLayout layout = new RelativeLayout(getContext());
		layout.setLayoutParams(new RelativeLayout.LayoutParams(
				RelativeLayout.LayoutParams.MATCH_PARENT,
				RelativeLayout.LayoutParams.MATCH_PARENT));
		layout.setClickable(true);
		layout.setOnClickListener(clickListener);
		MonthView m = new MonthView(getContext());

		RelativeLayout.LayoutParams lp = new RelativeLayout.LayoutParams(
				RelativeLayout.LayoutParams.MATCH_PARENT,
				RelativeLayout.LayoutParams.WRAP_CONTENT);
		lp.addRule(RelativeLayout.ALIGN_PARENT_TOP, RelativeLayout.TRUE);
		m.setLayoutParams(lp);
		m.setDays(days, height);
		m.setFocusable(false);
		m.setOnDayClickListener(this);
		layout.addView(m);
		if (first) {
			addView(layout, 0);
		} else {
			addView(layout);
		}
		return m;
	}

	public void setDate(List<CalendarDays> listDays, int height) {
		this.listDays = listDays;
		removeAllViews();
		for (CalendarDays days : listDays) {
			addMonth(days, height, false);
		}
		setToScreen(12);
	}

	public void gotoCurrentMonth() {
		setToScreen(12);
	}

	public List<CalendarDays> getDate() {
		return listDays;
	}

	public void SetOnCalendarChange(OnCalendarChange calendarChange) {
		this.calendarChange = calendarChange;
	}

	@Override
	public void onScreenChange(View v, int screen) {
		if (calendarChange != null) {
			calendarChange.calendarChanged(listDays.get(screen));
		}

	}

	@Override
	public void onDayClick(Day day) {
		if (dayClickListener != null) {
			dayClickListener.onDayClick(day);
		}

	}

}
