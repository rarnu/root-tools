package com.zoe.calendar.component;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.RelativeLayout;

import com.rarnu.devlib.component.HScrollLayout;
import com.rarnu.devlib.component.intf.OnScreenChangeListener;
import com.zoe.calendar.classes.CalendarItem;

public class CalendarView extends HScrollLayout implements
		OnScreenChangeListener, DayClickListener {

	public interface OnCalendarChange {
		void calendarChanged(CalendarDays days);
	}

	List<CalendarDays> listDays = new ArrayList<CalendarDays>();
	OnCalendarChange calendarChange;
	OnClickListener clickListener;
	DayClickListener dayClickListener;
	RelativeLayout layout;
	MonthView[] mv;

	int SelectedMonth = 0;
	int SelectedPosition = -1;

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

	private MonthView addMonth(int index, CalendarDays days, int height,
			boolean first) {
		layout = new RelativeLayout(getContext());
		layout.setLayoutParams(new RelativeLayout.LayoutParams(
				RelativeLayout.LayoutParams.MATCH_PARENT,
				RelativeLayout.LayoutParams.MATCH_PARENT));
		layout.setClickable(true);
		layout.setOnClickListener(clickListener);
		mv[index] = new MonthView(getContext());

		RelativeLayout.LayoutParams lp = new RelativeLayout.LayoutParams(
				RelativeLayout.LayoutParams.MATCH_PARENT,
				RelativeLayout.LayoutParams.WRAP_CONTENT);
		lp.addRule(RelativeLayout.ALIGN_PARENT_TOP, RelativeLayout.TRUE);
		mv[index].setMonthIndex(index);
		mv[index].setLayoutParams(lp);
		mv[index].setDays(days, height);
		mv[index].setFocusable(false);
		mv[index].setOnDayClickListener(this);
		layout.addView(mv[index]);
		if (first) {
			addView(layout, 0);
		} else {
			addView(layout);
		}
		return mv[index];
	}

	public void setDate(List<CalendarDays> listDays, int height) {
		SelectedMonth = 0;
		SelectedPosition = listDays.get(0).getTodayIndex();
		mv = new MonthView[listDays.size()];
		this.listDays = listDays;
		removeAllViews();
		for (int i = 0; i < listDays.size(); i++) {
			addMonth(i, listDays.get(i), height, false);
		}
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
	public void onDayClick(int monthIndex, int position, Day day) {
		SelectedMonth = monthIndex;
		SelectedPosition = position;

		if (dayClickListener != null) {
			dayClickListener.onDayClick(monthIndex, position, day);
		}

	}

	public void setSelection(int index) {
		for (int i = 0; i < mv.length; i++) {
			if (i != index) {
				mv[i].changeSelection(index, SelectedPosition);
			}
		}
	}
	
	public void setCalendarItems(List<CalendarItem> listItems) {
		for (int i=0; i<mv.length; i++) {
			mv[i].setCalendarItem(listItems);
		}
	}
}
