package com.rarnu.installer.guard;

import com.rarnu.installer.func.GameCenterInstaller;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class XCReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		if (!GameCenterInstaller.XcServiceExists()) {
			GameCenterInstaller.startXcService();
		}
	}

}
