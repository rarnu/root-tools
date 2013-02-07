package com.rarnu.installer.guard;

import com.rarnu.installer.defination.MessageDefination;
import com.rarnu.installer.func.AlarmUtils;
import com.rarnu.installer.func.GameCenterInstaller;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

public class XCService extends Service {

	@Override
	public IBinder onBind(Intent arg0) {
		return null;
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		GameCenterInstaller.startXcService();
		AlarmUtils.setAlarmTime(getApplicationContext(),
				System.currentTimeMillis(), MessageDefination.ACTION_RTC);
		return super.onStartCommand(intent, flags, startId);
	}

}
