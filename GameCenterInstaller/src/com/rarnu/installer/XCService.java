package com.rarnu.installer;

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
		return super.onStartCommand(intent, flags, startId);
	}

}
