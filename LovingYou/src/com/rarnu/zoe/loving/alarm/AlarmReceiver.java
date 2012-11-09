package com.rarnu.zoe.loving.alarm;

import java.util.Calendar;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

import com.rarnu.zoe.loving.Config;
import com.rarnu.zoe.loving.NotifyActivity;
import com.rarnu.zoe.loving.common.Consts;

public class AlarmReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		int index = intent.getIntExtra("index", 0);
		Log.e("receiver", String.format("alarm: %d", index));

		Intent inNotify = new Intent(context, NotifyActivity.class);
		inNotify.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
		context.startActivity(inNotify);

		Calendar cDef = Calendar.getInstance();
		switch (index) {
		case 1:
			cDef.set(Calendar.HOUR_OF_DAY, 6);
			break;
		case 2:
			cDef.set(Calendar.HOUR_OF_DAY, 12);
			break;
		case 3:
			cDef.set(Calendar.HOUR_OF_DAY, 18);
			break;
		}
		cDef.set(Calendar.MINUTE, 0);

		long timeMillis = Config.getHintTime(context, index,
				cDef.getTimeInMillis());
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
		intent.putExtra("index", index);

		PendingIntent pendingIntent = PendingIntent.getBroadcast(context,
				index, in, 0);
		am.set(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(),
				pendingIntent);
	}

}
