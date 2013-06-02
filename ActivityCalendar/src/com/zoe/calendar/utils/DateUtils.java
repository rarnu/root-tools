package com.zoe.calendar.utils;

import java.util.Calendar;

import android.text.format.DateFormat;

public class DateUtils {
	public static int daysBetween(Calendar cNow, Calendar cReturnDate) {
		setTimeToMidnight(cNow);
		setTimeToMidnight(cReturnDate);
		long todayMs = cNow.getTimeInMillis();
		long returnMs = cReturnDate.getTimeInMillis();
		long intervalMs = todayMs - returnMs;
		return millisecondsToDays(intervalMs);
	}

	public static int hoursBetween(Calendar cNow, Calendar cReturnHour) {
		long nowMs = cNow.getTimeInMillis();
		long returnMs = cReturnHour.getTimeInMillis();
		long intervalMs = nowMs - returnMs;
		return millissecondsToHours(intervalMs);
	}

	private static int millisecondsToDays(long intervalMs) {
		return (int) (intervalMs / (1000 * 86400));
	}

	private static int millissecondsToHours(long intervalMs) {
		return (int) (intervalMs / (1000 * 3600));
	}

	private static void setTimeToMidnight(Calendar calendar) {
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.SECOND, 0);
	}

	public static String formatDate(Calendar c) {
		return DateFormat.format("yyyy-MM-dd", c).toString();
	}

	public static String formatTime(Calendar c) {
		return DateFormat.format("kk:mm", c).toString();
	}
}
