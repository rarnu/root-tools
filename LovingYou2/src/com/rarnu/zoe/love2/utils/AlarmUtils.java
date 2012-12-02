package com.rarnu.zoe.love2.utils;

import java.util.Calendar;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

import com.rarnu.zoe.love2.common.Consts;

public class AlarmUtils {

	public static void startAlarm(Context context, int index, int hour,
			int minute) {
		Log.e("startAlarm", String.format("hour:%d, minute:%d", hour, minute));
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

		Intent intent = new Intent(Consts.ALARM_ACTION);
		intent.putExtra("index", index);
		PendingIntent pendingIntent = PendingIntent.getBroadcast(context,
				index, intent, 0);
		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);
		am.set(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(),
				pendingIntent);
	}

	public static void cancelAlarm(Context context, int index) {
		Intent intent = new Intent(Consts.ALARM_ACTION);
		intent.putExtra("index", index);
		PendingIntent sender = PendingIntent.getBroadcast(context, index,
				intent, 0);
		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);
		am.cancel(sender);
	}
}
