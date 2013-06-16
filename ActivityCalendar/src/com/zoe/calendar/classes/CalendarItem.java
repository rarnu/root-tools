package com.zoe.calendar.classes;

import android.util.Log;

public class CalendarItem {

	public int year;
	public int month;
	public int day;
	public String text;

	public static CalendarItem fromRemote(RemoteCalendarItem item) {
		CalendarItem ci = new CalendarItem();
		// 2013-09-20
		// 0123456789
		String year = item.date.substring(0, 4);
		String month = item.date.substring(5, 7);
		String day = item.date.substring(8);

		ci.year = Integer.parseInt(year);
		ci.month = Integer.parseInt(month) - 1;
		ci.day = Integer.parseInt(day);

		ci.text = item.text;
		Log.e("CalendarItem", String.format(
				"year:%d, month:%d, day:%d, text:%s", ci.year, ci.month,
				ci.day, ci.text));
		return ci;
	}
}
