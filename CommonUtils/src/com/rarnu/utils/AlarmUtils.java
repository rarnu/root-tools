package com.rarnu.utils;

import java.util.Calendar;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;

public class AlarmUtils {

	public static void startAlarm(Context context, int type, int index,
			int hour, int minute, String action) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeInMillis(System.currentTimeMillis());
		calendar.set(Calendar.HOUR_OF_DAY, hour);
		calendar.set(Calendar.MINUTE, minute);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);

		if (calendar.getTimeInMillis() < System.currentTimeMillis()) {
			calendar.setTimeInMillis(System.currentTimeMillis()
					+ (24 * 60 * 60 * 1000));
			calendar.set(Calendar.HOUR_OF_DAY, hour);
			calendar.set(Calendar.MINUTE, minute);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MILLISECOND, 0);
		}

		Intent intent = new Intent(action);
		intent.putExtra("index", index);
		PendingIntent pendingIntent = PendingIntent.getBroadcast(context,
				index, intent, 0);
		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);
		am.set(type, calendar.getTimeInMillis(), pendingIntent);
	}

	public static void startAlarm(Context context, int type, int index,
			int interval, String action) {
		Intent intent = new Intent(action);
		intent.putExtra("index", index);
		PendingIntent pendingIntent = PendingIntent.getBroadcast(context,
				index, intent, 0);
		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);
		am.setRepeating(type, System.currentTimeMillis(), interval,
				pendingIntent);
	}

	public static void cancelAlarm(Context context, int index, String action) {
		Intent intent = new Intent(action);
		intent.putExtra("index", index);
		PendingIntent sender = PendingIntent.getBroadcast(context, index,
				intent, 0);
		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);
		am.cancel(sender);
	}
}
