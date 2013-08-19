package com.rarnu.battery.notifier.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.rarnu.battery.notifier.utils.CommandUtils;

public class PowerReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		String action = intent.getAction();
		if (action != null) {
			if (action.equals(Intent.ACTION_POWER_CONNECTED)) {
				CommandUtils.sendStartChargeCommand();
			}
		}

	}

}
