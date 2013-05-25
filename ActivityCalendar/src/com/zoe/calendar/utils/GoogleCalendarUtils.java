package com.zoe.calendar.utils;

import java.util.Calendar;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.provider.CalendarContract.Calendars;
import android.provider.CalendarContract.Events;

import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.classes.GoogleCalendar;

public class GoogleCalendarUtils {

	public static GoogleCalendar getCalendars(Context context) {
		GoogleCalendar gc = null;
		String[] projection = new String[] { Calendars._ID,
				Calendars.OWNER_ACCOUNT };
		Uri calendars = Calendars.CONTENT_URI;
		Cursor c = context.getContentResolver().query(calendars, projection,
				null, null, null);
		if (c != null) {
			c.moveToFirst();
			String account = "";
			while (!c.isAfterLast()) {
				account = c.getString(1);
				if (account.endsWith("gmail.com")) {
					gc = new GoogleCalendar();
					gc._id = c.getInt(0);
					gc.account = account;
					break;
				}
				c.moveToNext();
			}
			c.close();
		}

		return gc;

	}

	public static void addEvent(Context context, GoogleCalendar gc,
			ActivityItem item) {
		if (!eventExists(context, gc, item)) {
			ContentValues values = buildContentValues(gc, item);
			context.getContentResolver().insert(Events.CONTENT_URI, values);
		}
	}

	public static void editEvent(Context context, GoogleCalendar gc,
			ActivityItem oldItem, ActivityItem newItem) {
		ContentValues values = buildContentValues(gc, newItem);
		context.getContentResolver().update(Events.CONTENT_URI, values,
				String.format("%s=?", Events.TITLE),
				new String[] { buildUniqueTitle(oldItem) });
	}

	public static void deleteEvent(Context context, GoogleCalendar gc,
			ActivityItem item) {
		context.getContentResolver().delete(Events.CONTENT_URI,
				String.format("%s=?", Events.TITLE),
				new String[] { buildUniqueTitle(item) });
	}

	public static boolean eventExists(Context context, GoogleCalendar gc,
			ActivityItem item) {
		Cursor c = context.getContentResolver().query(Events.CONTENT_URI, null,
				String.format("%s=?", Events.TITLE),
				new String[] { buildUniqueTitle(item) }, null);
		if (c == null) {
			return false;
		} else {
			c.moveToFirst();
			boolean ret = c.isAfterLast();
			c.close();
			return !ret;
		}
	}

	private static ContentValues buildContentValues(GoogleCalendar gc,
			ActivityItem item) {
		ContentValues values = new ContentValues();
		Calendar cStart = Calendar.getInstance();

		cStart.set(Calendar.YEAR, item.year);
		cStart.set(Calendar.MONTH, item.startMonth - 1);
		cStart.set(Calendar.DAY_OF_MONTH, item.startDay);
		if (item.startHour == -1 && item.endHour == -1) {
			values.put(Events.ALL_DAY, 1);
		} else {
			cStart.set(Calendar.HOUR_OF_DAY, item.startHour);
			cStart.set(Calendar.MINUTE, item.startMinute);
		}

		Calendar cEnd = (Calendar) cStart.clone();
		if (item.endMonth != -1) {
			cEnd.set(Calendar.MONTH, item.endMonth - 1);
			cEnd.set(Calendar.DAY_OF_MONTH, item.endDay);
		}
		if (item.endHour != -1) {
			cEnd.set(Calendar.HOUR_OF_DAY, item.endHour);
			cEnd.set(Calendar.MINUTE, item.endMinute);
		}

		if (cEnd.equals(cStart)) {
			cEnd.set(Calendar.HOUR_OF_DAY, cStart.get(Calendar.HOUR_OF_DAY) + 2);
		}

		values.put(Events.DTSTART, cStart.getTimeInMillis());
		values.put(Events.DTEND, cEnd.getTimeInMillis());
		values.put(Events.TITLE, buildUniqueTitle(item));
		values.put(Events.DESCRIPTION, item.content);
		values.put(Events.HAS_ALARM, 1);
		values.put(Events.EVENT_LOCATION, item.location);
		values.put(Events.CALENDAR_ID, gc._id);
		values.put(Events.EVENT_TIMEZONE, "China/Beijing");

		return values;
	}

	private static String buildUniqueTitle(ActivityItem item) {
		return String.format("[%d.%d.%d]%s", item.year, item.startMonth,
				item.startDay, item.title);
	}
}
