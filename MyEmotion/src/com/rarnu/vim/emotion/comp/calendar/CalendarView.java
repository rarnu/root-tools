package com.rarnu.vim.emotion.comp.calendar;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.RelativeLayout;

import com.rarnu.vim.emotion.EmotionInterface;
import com.rarnu.vim.emotion.comp.OnScreenChangeListener;
import com.rarnu.vim.emotion.comp.OnScreenTouchListener;
import com.rarnu.vim.emotion.comp.VScrollLayout;

public class CalendarView extends VScrollLayout implements
		OnScreenTouchListener, OnScreenChangeListener {

	List<CalendarDays> listDays = new ArrayList<CalendarDays>();
	MonthView mCurrent, mPrior, mNext;
	OnItemClickListener listener;
	OnCalendarChange calendarChange;

	OnClickListener clickListener;

	public CalendarView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public CalendarView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	private void init() {
		setOnScreenTouchListener(this);
		setOnScreenChangeListener(this);
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
		lp.addRule(RelativeLayout.CENTER_IN_PARENT, RelativeLayout.TRUE);
		m.setLayoutParams(lp);
		m.setDays(days, height);
		m.setFocusable(false);
		m.setOnItemClickListener(listener);
		layout.addView(m);
		if (first) {
			addView(layout, 0);
		} else {
			addView(layout);
		}
		return m;
	}

	public void setOnCalendarClick(OnItemClickListener listener) {
		this.listener = listener;
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
	public void onActionScrolling(View v) {
		((EmotionInterface) getContext()).setLeftRightScrollable(false);
	}

	@Override
	public void onActionReset(View v) {
		((EmotionInterface) getContext()).setLeftRightScrollable(true);
	}

	@Override
	public void onScreenChange(View v, int screen) {
		if (calendarChange != null) {
			calendarChange.calendarChanged(listDays.get(screen));
		}

	}

	public void setViewClick(OnClickListener clickListener) {
		this.clickListener = clickListener;
	}

}
