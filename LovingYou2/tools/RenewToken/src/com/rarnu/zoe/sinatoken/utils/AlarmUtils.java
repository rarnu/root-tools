package com.rarnu.zoe.sinatoken.utils;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

import com.rarnu.zoe.sinatoken.Consts;

public class AlarmUtils {

	public static void startAlarm(Context context, long second) {
		// second = 10;
		Log.e("startAlarm", String.format("second:%d", second));

		Intent intent = new Intent(Consts.ALARM_ACTION);
		PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0,
				intent, 0);
		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);

		am.set(AlarmManager.RTC_WAKEUP, System.currentTimeMillis() + second
				* 1000, pendingIntent);
	}

	public static void cancelAlarm(Context context) {
		Intent intent = new Intent(Consts.ALARM_ACTION);
		PendingIntent sender = PendingIntent.getBroadcast(context, 0,
				intent, 0);
		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);
		am.cancel(sender);
	}
}
