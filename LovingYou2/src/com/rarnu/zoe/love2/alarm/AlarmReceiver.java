package com.rarnu.zoe.love2.alarm;

import java.util.Calendar;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.rarnu.zoe.love2.Global;
import com.rarnu.zoe.love2.common.Config;
import com.rarnu.zoe.love2.common.Consts;
import com.rarnu.zoe.love2.utils.NotificationUtils;

public class AlarmReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {

		Consts.setTaskTexts(context);
		int day = Global.database.getDay();
		NotificationUtils.doHint(context, Consts.taskTitle[day - 1]);

		Calendar cDef = Calendar.getInstance();
		cDef.set(Calendar.HOUR_OF_DAY, 11);
		cDef.set(Calendar.MINUTE, 0);

		long timeMillis = Config
				.getHintTime(context, 1, cDef.getTimeInMillis());
		Calendar cNew = Calendar.getInstance();
		cNew.setTimeInMillis(timeMillis);

		AlarmManager am = (AlarmManager) context
				.getSystemService(Context.ALARM_SERVICE);

		Calendar calendar = Calendar.getInstance();

		calendar.setTimeInMillis(System.currentTimeMillis()
				+ (24 * 60 * 60 * 1000));
		calendar.set(Calendar.HOUR_OF_DAY, cNew.get(Calendar.HOUR_OF_DAY));
		calendar.set(Calendar.MINUTE, cNew.get(Calendar.MINUTE));
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		Intent in = new Intent(Consts.ALARM_ACTION);
		intent.putExtra("index", 1);

		PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 1,
				in, 0);
		am.set(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(),
				pendingIntent);
	}

}
