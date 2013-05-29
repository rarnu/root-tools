package com.zoe.calendar.database;

import java.util.ArrayList;
import java.util.List;

import android.content.ContentResolver;
import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;

import com.zoe.calendar.classes.ActivityItem;

public class QueryUtils {

	public static void mergeData(Context context, List<ActivityItem> list)
			throws Exception {
		// merge data
		ContentResolver cr = context.getContentResolver();
		for (ActivityItem item : list) {
			ContentValues values = new ContentValues();
			values.put("_id", item._id);
			values.put("city", item.city);
			values.put("year", item.year);
			values.put("start_month", item.startMonth);
			values.put("start_day", item.startDay);
			values.put("end_month", item.endMonth);
			values.put("end_day", item.endDay);
			values.put("start_hour", item.startHour);
			values.put("start_minute", item.startMinute);
			values.put("end_hour", item.endHour);
			values.put("end_minute", item.endMinute);
			values.put("title", item.title);
			values.put("url", item.url);
			values.put("source", item.source);
			values.put("location", item.location);
			values.put("weight", item.weight);
			values.put("tags", item.tags);
			values.put("content", item.content);
			values.put("status", item.status);
			cr.insert(ActivityProvider.CONTENT_URI, values);
		}

	}

	public static List<ActivityItem> queryActivity(Context context,
			String city, int year, int month, int day, int status)
			throws Exception {
		Cursor c = context
				.getContentResolver()
				.query(ContentUris.withAppendedId(ActivityProvider.CONTENT_URI,
						ActivityProvider.ACTIONID_QUERY),
						null,
						"city=? and year=? and ((start_month=? and start_day=?) or (end_month=? and end_day=?)) and status=?",
						new String[] { city, String.valueOf(year),
								String.valueOf(month), String.valueOf(day),
								String.valueOf(month), String.valueOf(day),
								String.valueOf(status) }, null);
		List<ActivityItem> list = null;
		if (c != null) {
			list = new ArrayList<ActivityItem>();
			c.moveToFirst();
			while (!c.isAfterLast()) {
				ActivityItem item = new ActivityItem();
				item._id = c.getInt(0);
				item.city = c.getString(1);
				item.year = c.getInt(2);
				item.startMonth = c.getInt(3);
				item.startDay = c.getInt(4);
				item.endMonth = c.getInt(5);
				item.endDay = c.getInt(6);
				item.startHour = c.getInt(7);
				item.startMinute = c.getInt(8);
				item.endHour = c.getInt(9);
				item.endMinute = c.getInt(10);
				item.title = c.getString(11);
				item.url = c.getString(12);
				item.source = c.getString(13);
				item.location = c.getString(14);
				item.weight = c.getInt(15);
				item.tags = c.getString(16);
				item.content = c.getString(17);
				list.add(item);
				c.moveToNext();
			}
			c.close();
		}
		return list;
	}

	public static int deleteActivity(Context context, int id) throws Exception {
		return updateActivity(context, id, 0);
	}

	public static int restoreActivity(Context context, int id) throws Exception {
		return updateActivity(context, id, 1);
	}

	private static int updateActivity(Context context, int id, int status)
			throws Exception {
		ContentValues cv = new ContentValues();
		cv.put("status", status);
		return context.getContentResolver().update(
				ActivityProvider.CONTENT_URI, cv, null,
				new String[] { String.valueOf(id) });
	}
}
