package com.zoe.calendar.component;

import java.util.Calendar;

import com.zoe.calendar.utils.DateUtils;

public class Day {

	public int year = 0;
	public int month = 0;
	public int day = 0;
	public String chineseDay = "";
	public boolean highlight = false;
	public boolean today = false;
	public boolean selected = false;
	public String image = "";
	public Object data = null;

	public boolean isBeforeToday() {
		Calendar cToday = Calendar.getInstance();
		Calendar cDay = Calendar.getInstance();
		cDay.set(Calendar.YEAR, year);
		cDay.set(Calendar.MONTH, month);
		cDay.set(Calendar.DAY_OF_MONTH, day);
		return cDay.getTimeInMillis() < cToday.getTimeInMillis();
	}

	public boolean isAfter60Days() {
		Calendar cToday = Calendar.getInstance();
		Calendar cDay = Calendar.getInstance();
		cDay.set(Calendar.YEAR, year);
		cDay.set(Calendar.MONTH, month);
		cDay.set(Calendar.DAY_OF_MONTH, day);
		int daysBetween = Math.abs(DateUtils.daysBetween(cToday, cDay));
		return daysBetween > 60;
	}

}
