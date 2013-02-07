package com.rarnu.installer.func;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;

public class AlarmUtils {

	public static void setAlarmTime(Context context, long timeInMillis,
			String action) {
		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);
		Intent intent = new Intent(action);
		PendingIntent sender = PendingIntent.getBroadcast(context, 0, intent,
				PendingIntent.FLAG_CANCEL_CURRENT);
		int interval = 1000 * 15;
		am.setRepeating(AlarmManager.RTC_WAKEUP, timeInMillis, interval, sender);
	}
}
