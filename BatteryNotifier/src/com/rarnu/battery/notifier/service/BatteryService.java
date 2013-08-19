package com.rarnu.battery.notifier.service;

import android.app.Service;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.IBinder;

public class BatteryService extends Service {

	BatteryReceiver receiver;
	IntentFilter filter;

	@Override
	public IBinder onBind(Intent intent) {
		return null;
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		return super.onStartCommand(intent, flags, startId);
	}

	@Override
	public void onCreate() {
		super.onCreate();
		filter = new IntentFilter(Intent.ACTION_BATTERY_CHANGED);
		receiver = new BatteryReceiver();
		try {
			registerReceiver(receiver, filter);
		} catch (Exception e) {

		}
	}

	@Override
	public void onDestroy() {
		try {
			unregisterReceiver(receiver);
		} catch (Exception e) {

		}
		super.onDestroy();
	}
}
