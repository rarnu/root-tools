package com.zoe.calendar.component;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import android.content.Context;

public class CalendarDays {

	public List<Day> lstDays = new ArrayList<Day>();
	public int year = 0, month = 0;
	public int lines = 0;
	private Context context;
	private int todayIndex = -1;

	public CalendarDays(Context context) {
		this.context = context;
		Calendar c = Calendar.getInstance();
		int year = c.get(Calendar.YEAR);
		int month = c.get(Calendar.MONDAY);
		setDate(year, month);
	}

	public void setDate(int year, int month) {
		this.year = year;
		this.month = month;

		Calendar cToday = Calendar.getInstance();
		Calendar c = Calendar.getInstance();

		c.set(Calendar.YEAR, year);
		c.set(Calendar.MONTH, month);
		c.set(Calendar.DAY_OF_MONTH, 1);
		c.setFirstDayOfWeek(Calendar.MONDAY);

		LunarCalendar lc = new LunarCalendar(context);

		int day = c.get(Calendar.DAY_OF_WEEK) - 1;
		day = day <= 0 ? 7 + day : day;
		int dateOfMonth = c.getActualMaximum(Calendar.DATE);

		lstDays.clear();
		for (int i = 0; i < day - 1; i++) {
			lstDays.add(new Day());
		}
		for (int i = 0; i < dateOfMonth; i++) {
			Day item = new Day();
			item.year = year;
			item.month = month;
			item.day = i + 1;
			item.highlight = (lstDays.size() % 7 == 5)
					|| (lstDays.size() % 7 == 6);
			item.today = (cToday.get(Calendar.MONTH) == item.month)
					&& (cToday.get(Calendar.DAY_OF_MONTH) == item.day);
			item.selected = item.today;
			if (item.today) {
				todayIndex = lstDays.size();
			}
			LunarCalendarConvertUtil.parseLunarCalendar(item.year, item.month, item.day, lc);
			item.chineseDay = lc.getChinaDayString(true);
			lstDays.add(item);
		}

		while (lstDays.size() % 7 != 0) {
			lstDays.add(new Day());
		}

		lines = lstDays.size() > 35 ? 6 : 5;
	}

	public int getTodayIndex() {
		return todayIndex;
	}
	
	public CalendarDays getPriorMonth() {
		int newMonth = month - 1;
		int newYear = year;

		if (newMonth < 0) {
			newMonth = 11;
			newYear = newYear - 1;
		}
		CalendarDays result = new CalendarDays(context);
		result.setDate(newYear, newMonth);
		return result;
	}

	public CalendarDays getNextMonth() {
		int newMonth = month + 1;
		int newYear = year;

		if (newMonth > 11) {
			newMonth = 0;
			newYear = newYear + 1;
		}
		CalendarDays result = new CalendarDays(context);
		result.setDate(newYear, newMonth);
		return result;
	}
}
