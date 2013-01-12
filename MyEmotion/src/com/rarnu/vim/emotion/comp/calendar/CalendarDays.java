package com.rarnu.vim.emotion.comp.calendar;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

public class CalendarDays {

	public List<Day> lstDays = new ArrayList<Day>();
	public int year = 0, month = 0;

	public CalendarDays() {
		Calendar c = Calendar.getInstance();
		int year = c.get(Calendar.YEAR);
		int month = c.get(Calendar.MONDAY);
		setDate(year, month);
	}

	public void setDate(int year, int month) {
		this.year = year;
		this.month = month;
		Calendar c = Calendar.getInstance();
		c.set(Calendar.YEAR, year);
		c.set(Calendar.MONTH, month);
		c.set(Calendar.DAY_OF_MONTH, 1);
		c.setFirstDayOfWeek(Calendar.SUNDAY);
		int day = c.get(Calendar.DAY_OF_WEEK);
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
			lstDays.add(item);
		}

		while (lstDays.size() % 7 != 0) {
			lstDays.add(new Day());
		}

	}

	public CalendarDays getPriorMonth() {
		int newMonth = month - 1;
		int newYear = year;

		if (newMonth < 0) {
			newMonth = 11;
			newYear = newYear - 1;
		}
		CalendarDays result = new CalendarDays();
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
		CalendarDays result = new CalendarDays();
		result.setDate(newYear, newMonth);
		return result;
	}
}
