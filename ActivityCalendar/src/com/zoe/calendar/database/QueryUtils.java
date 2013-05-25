package com.zoe.calendar.database;

import java.util.ArrayList;
import java.util.List;

import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;

import com.zoe.calendar.classes.ActivityItem;

public class QueryUtils {

	public static List<ActivityItem> queryActivity(Context context, int year,
			int month, int day) {
		Cursor c = context
				.getContentResolver()
				.query(ContentUris.withAppendedId(ActivityProvider.CONTENT_URI,
						ActivityProvider.ACTIONID_QUERY),
						null,
						"year=? and ((start_month=? and start_day=?) or (end_month=? and end_day=?)) and status=1",
						new String[] { String.valueOf(year),
								String.valueOf(month), String.valueOf(day),
								String.valueOf(month), String.valueOf(day) },
						null);
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

	public static int deleteActivity(Context context, int id) {
		return context.getContentResolver().delete(
				ContentUris.withAppendedId(ActivityProvider.CONTENT_URI, id),
				null, null);
	}
}
