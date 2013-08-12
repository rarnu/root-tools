package com.sbbs.me.android.service;

import com.rarnu.utils.AlarmUtils;
import com.sbbs.me.android.consts.Actions;

import android.app.AlarmManager;
import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

public class MessageService extends Service {

	@Override
	public IBinder onBind(Intent intent) {
		return null;
	}

	@Override
	public void onCreate() {
		super.onCreate();
		AlarmUtils.startAlarm(getApplicationContext(), AlarmManager.RTC, 0,
				60 * 1000, Actions.ACTION_CHECK_MESSAGE);
		AlarmUtils.startAlarm(getApplicationContext(), AlarmManager.RTC, 1,
				60 * 1000 * 60 * 12, Actions.ACTION_CHECK_UPDATE);
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		return super.onStartCommand(intent, flags, startId);
	}

	@Override
	public void onDestroy() {
		AlarmUtils.cancelAlarm(getApplicationContext(), 0,
				Actions.ACTION_CHECK_MESSAGE);
		AlarmUtils.cancelAlarm(getApplicationContext(), 1,
				Actions.ACTION_CHECK_UPDATE);
		super.onDestroy();
	}

}
